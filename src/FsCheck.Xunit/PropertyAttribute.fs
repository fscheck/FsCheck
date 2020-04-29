namespace FsCheck.Xunit

open System
open System.Threading.Tasks

open FsCheck
open Xunit
open Xunit.Sdk
open Xunit.Abstractions

type PropertyFailedException =
    inherit Exception
    new (testResult:FsCheck.TestResult) = {
        inherit Exception(sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" testResult)) }
    new (userMessage, innerException : exn) = {
        inherit Exception(userMessage, innerException) }

//can not be an anonymous type because of let mutable.
type XunitRunner() =
    let mutable result = None
    member __.Result = result.Value
    interface IRunner with
        override __.OnStartFixture _ = ()
        override __.OnArguments (ntest,args, every) =
            every ntest args |> ignore
        override __.OnShrink(args, everyShrink) =
            everyShrink args |> ignore
        override __.OnFinished(_ ,testResult) =
            result <- Some testResult

type internal PropertyConfig =
    { MaxTest        : Option<int>
      MaxRejected    : Option<int>
      Replay         : Option<string>
      Parallelism    : Option<int>
      StartSize      : Option<int>
      EndSize        : Option<int>
      Verbose        : Option<bool>
      QuietOnSuccess : Option<bool>
      Arbitrary      : Type[] }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal PropertyConfig = 
    let orElse y = function
        | Some x -> Some x
        | None   -> y

    let orDefault x y = defaultArg y x

    let zero =
        { MaxTest        = None
          MaxRejected    = None
          Replay         = None
          Parallelism    = None
          StartSize      = None
          EndSize        = None
          Verbose        = None
          QuietOnSuccess = None
          Arbitrary      = [||] }

    let combine extra original =
        { MaxTest        = extra.MaxTest        |> orElse original.MaxTest
          MaxRejected    = extra.MaxRejected    |> orElse original.MaxRejected
          Replay         = extra.Replay         |> orElse original.Replay
          Parallelism    = extra.Parallelism    |> orElse original.Parallelism
          StartSize      = extra.StartSize      |> orElse original.StartSize
          EndSize        = extra.EndSize        |> orElse original.EndSize
          Verbose        = extra.Verbose        |> orElse original.Verbose
          QuietOnSuccess = extra.QuietOnSuccess |> orElse original.QuietOnSuccess
          Arbitrary      = Array.append extra.Arbitrary original.Arbitrary }

    let parseReplay (str: string) =
        //if someone sets this, we want it to throw if it fails
        let split = str.Trim('(',')').Split([|","|], StringSplitOptions.RemoveEmptyEntries)
        let seed = UInt64.Parse(split.[0])
        let gamma = UInt64.Parse(split.[1])
        let size = if split.Length = 3 then Some <| Convert.ToInt32(UInt32.Parse(split.[2])) else None
        { Rnd = Rnd (seed,gamma); Size = size }

    let toConfig (output : TestOutputHelper) propertyConfig =
        Config.Default
              .WithReplay(
                  propertyConfig.Replay
                  |> Option.map parseReplay
                  |> orElse Config.Default.Replay
              )
              .WithParallelRunConfig(
                  propertyConfig.Parallelism
                  |> Option.map (fun i -> { MaxDegreeOfParallelism = i })
                  |> orElse Config.Default.ParallelRunConfig
              )
              .WithMaxTest(propertyConfig.MaxTest |> orDefault Config.Default.MaxTest)
              .WithMaxRejected(propertyConfig.MaxRejected |> orDefault Config.Default.MaxRejected)
              .WithStartSize(propertyConfig.StartSize |> orDefault Config.Default.StartSize)
              .WithEndSize(propertyConfig.EndSize |> orDefault Config.Default.EndSize)
              .WithQuietOnSuccess(propertyConfig.QuietOnSuccess |> orDefault Config.Default.QuietOnSuccess)
              .WithArbitrary(Seq.toList propertyConfig.Arbitrary)
              .WithRunner(XunitRunner())
              .WithEvery(
                  if propertyConfig.Verbose |> Option.exists id then 
                      fun n args -> output.WriteLine (Config.Verbose.Every n args); ""
                  else 
                      Config.Quick.Every
              )
              .WithEveryShrink(
                  if propertyConfig.Verbose |> Option.exists id then 
                      fun args -> output.WriteLine (Config.Verbose.EveryShrink args); ""
                  else 
                      Config.Quick.EveryShrink
              )

///Run this method as an FsCheck test.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<XunitTestCaseDiscoverer("FsCheck.Xunit.PropertyDiscoverer", "FsCheck.Xunit")>]
type public PropertyAttribute() =
    inherit FactAttribute()
    let mutable config = PropertyConfig.zero
    let mutable replay = null
    let mutable parallelism = -1
    let mutable maxTest = -1
    let mutable maxRejected = -1
    let mutable startSize = -1
    let mutable endSize = -1
    let mutable verbose = false
    let mutable arbitrary = [||]
    let mutable quietOnSuccess = false
    
    ///If set, the seed to use to start testing. Allows reproduction of previous runs. You can just paste
    ///the tuple from the output window, e.g. 12344,12312 or (123,123).
    member __.Replay with get() = replay and set(v) = replay <- v; config <- {config with Replay = if String.IsNullOrEmpty v then None else Some v}
    ///If set, run tests in parallel. Useful for Task/async related work and heavy number crunching
    ///Environment.ProcessorCount have been found to be useful default.
    member __.Parallelism with get() = parallelism and set(v) = parallelism <- v; config <- {config with Parallelism = Some v}
    ///The maximum number of tests that are run.
    member __.MaxTest with get() = maxTest and set(v) = maxTest <- v; config <- {config with MaxTest = Some v}
    ///The maximum number of tests where values are rejected, e.g. as the result of ==>
    member __.MaxRejected with get() = maxRejected and set(v) = maxRejected <- v; config <- {config with MaxRejected = Some v}
    ///The size to use for the first test.
    member __.StartSize with get() = startSize and set(v) = startSize <- v; config <- {config with StartSize = Some v}
    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member __.EndSize with get() = endSize and set(v) = endSize <- v; config <- {config with EndSize = Some v}
    ///Output all generated arguments.
    member __.Verbose with get() = verbose and set(v) = verbose <- v; config <- {config with Verbose = Some v}
    ///The Arbitrary instances to use for this test method. The Arbitrary instances
    ///are merged in back to front order i.e. instances for the same generated type
    ///at the front of the array will override those at the back.
    member __.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v; config <- {config with Arbitrary = v}
    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
    ///test failures.
    member __.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v; config <- {config with QuietOnSuccess = Some v}

    member internal __.Config = config

///Set common configuration for all properties within this class or module
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type public PropertiesAttribute() = inherit PropertyAttribute()

/// The xUnit2 test runner for the PropertyAttribute that executes the test via FsCheck
type PropertyTestCase(diagnosticMessageSink:IMessageSink, defaultMethodDisplay:TestMethodDisplay, testMethod:ITestMethod, ?testMethodArguments:obj []) =
    inherit XunitTestCase(diagnosticMessageSink, defaultMethodDisplay, testMethod, (match testMethodArguments with | None -> null | Some v -> v))

    new() = new PropertyTestCase(null, TestMethodDisplay.ClassAndMethod, null)

    member this.Init(output:TestOutputHelper) =
        let factAttribute = this.TestMethod.Method.GetCustomAttributes(typeof<PropertyAttribute>) |> Seq.head
        let generalAttribute = 
            this.TestMethod.TestClass.Class.GetCustomAttributes(typeof<PropertiesAttribute>) 
                |> Seq.tryFind (fun _ -> true)

        let config =
            match generalAttribute with
            | Some generalAttribute ->
                PropertyConfig.combine
                    (factAttribute.GetNamedArgument "Config")
                    (generalAttribute.GetNamedArgument "Config")
            | None ->
                factAttribute.GetNamedArgument "Config"
        
        { config with Arbitrary = config.Arbitrary }
        |> PropertyConfig.toConfig output 

    override this.RunAsync(diagnosticMessageSink:IMessageSink, messageBus:IMessageBus, constructorArguments:obj [], aggregator:ExceptionAggregator, cancellationTokenSource:Threading.CancellationTokenSource) =
        let test = new XunitTest(this, this.DisplayName)
        let summary = new RunSummary(Total = 1);
        let outputHelper = new TestOutputHelper()
        outputHelper.Initialize(messageBus, test)
        let testExec() =
            
            let config = this.Init(outputHelper)
            let timer = ExecutionTimer()
            let result =
                try
                    let xunitRunner = if config.Runner :? XunitRunner then (config.Runner :?> XunitRunner) else XunitRunner()
                    let runMethod = this.TestMethod.Method.ToRuntimeMethod()
                    let target =
                        constructorArguments
                            |> Array.tryFind (fun x -> x :? TestOutputHelper)
                            |> Option.iter (fun x -> (x :?> TestOutputHelper).Initialize(messageBus, test))
                        let testClass = this.TestMethod.TestClass.Class.ToRuntimeType()
                        if this.TestMethod.TestClass <> null && not this.TestMethod.Method.IsStatic then
                            Some (test.CreateTestClass(testClass, constructorArguments, messageBus, timer, cancellationTokenSource))
                        else None

                    timer.Aggregate(fun () -> Check.Method(config, runMethod, ?target=target))

                    match xunitRunner.Result with
                          | TestResult.Passed _ ->
                            let output = Runner.onFinishedToString "" xunitRunner.Result
                            outputHelper.WriteLine(output)
                            new TestPassed(test, timer.Total, outputHelper.Output) :> TestResultMessage
                          | TestResult.Exhausted _ ->
                            summary.Failed <- summary.Failed + 1
                            upcast new TestFailed(test, timer.Total, outputHelper.Output, new PropertyFailedException(xunitRunner.Result))
                          | TestResult.Failed (testdata, originalArgs, shrunkArgs, Outcome.Failed e, originalSeed, lastSeed, lastSize)  ->
                            summary.Failed <- summary.Failed + 1
                            let message = sprintf "%s%s" Environment.NewLine (Runner.onFailureToString "" testdata originalArgs shrunkArgs originalSeed lastSeed lastSize)
                            upcast new TestFailed(test, timer.Total, outputHelper.Output, new PropertyFailedException(message, e))
                          | TestResult.Failed _ ->
                            summary.Failed <- summary.Failed + 1
                            upcast new TestFailed(test, timer.Total, outputHelper.Output, new PropertyFailedException(xunitRunner.Result))
                with
                    | ex ->
                      summary.Failed <- summary.Failed + 1
                      outputHelper.WriteLine("Exception during test")
                      upcast new TestFailed(test, timer.Total, outputHelper.Output, ex)

           
            messageBus.QueueMessage(result) |> ignore
            summary.Time <- summary.Time + result.ExecutionTime
            if not (messageBus.QueueMessage(new TestFinished(test, summary.Time, result.Output))) then
                cancellationTokenSource.Cancel() |> ignore
            if not (messageBus.QueueMessage(new TestCaseFinished(this, summary.Time, summary.Total, summary.Failed, summary.Skipped))) then
                cancellationTokenSource.Cancel() |> ignore

            summary

        if not (messageBus.QueueMessage(new TestCaseStarting(this))) then
            cancellationTokenSource.Cancel() |> ignore

        if not (messageBus.QueueMessage(new TestStarting(test))) then
            cancellationTokenSource.Cancel() |> ignore

        if not(String.IsNullOrEmpty(this.SkipReason)) then
            summary.Skipped <- summary.Skipped + 1
            if not(messageBus.QueueMessage(new TestSkipped(test, this.SkipReason))) then
                cancellationTokenSource.Cancel() |> ignore
            if not(messageBus.QueueMessage(new TestCaseFinished(this, decimal 1, 0, 0, 1))) then
                cancellationTokenSource.Cancel() |> ignore
            Task.FromResult(summary)
        else
            Task.Run(testExec)

/// xUnit2 test case discoverer to link the method with the PropertyAttribute to the PropertyTestCase
/// so the test can be run via FsCheck.
type PropertyDiscoverer(messageSink:IMessageSink) =

    new () = PropertyDiscoverer(null)

    member __.MessageSink = messageSink

    interface IXunitTestCaseDiscoverer with
        override this.Discover(discoveryOptions:ITestFrameworkDiscoveryOptions, testMethod:ITestMethod, _:IAttributeInfo)=
            let ptc = new PropertyTestCase(this.MessageSink, discoveryOptions.MethodDisplayOrDefault(), testMethod)
            Seq.singleton (ptc :> IXunitTestCase)

namespace rec FsCheck.Xunit

open System
open System.Diagnostics
open System.Reflection
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

open Xunit
open Xunit.Sdk
open Xunit.v3

open FsCheck

type PropertyFailedException =
    inherit Exception
    new (testResult:FsCheck.TestResult) = {
        inherit Exception(sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" testResult)) }
    new (userMessage, innerException : exn) = {
        inherit Exception(userMessage, innerException) }

//can not be an anonymous type because of let mutable.
type XunitRunner() =
    let mutable result = None
    member _.Result = result.Value
    interface IRunner with
        override _.OnStartFixture _ = ()
        override _.OnArguments (ntest,args, every) =
            every ntest args |> ignore
        override _.OnShrink(args, everyShrink) =
            everyShrink args |> ignore
        override _.OnFinished(_ ,testResult) =
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
[<XunitTestCaseDiscoverer(typeof<PropertyDiscoverer>)>]
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
    ///Additionally, you can also specify a start size as the third parameter, e.g. 12344,12312,10 or (123,123,10).
    member _.Replay with get() = replay and set(v) = replay <- v; config <- {config with Replay = if String.IsNullOrEmpty v then None else Some v}
    ///If set, run tests in parallel. Useful for Task/async related work and heavy number crunching
    ///Environment.ProcessorCount have been found to be useful default.
    member _.Parallelism with get() = parallelism and set(v) = parallelism <- v; config <- {config with Parallelism = Some v}
    ///The maximum number of tests that are run.
    member _.MaxTest with get() = maxTest and set(v) = maxTest <- v; config <- {config with MaxTest = Some v}
    ///The maximum number of tests where values are rejected, e.g. as the result of ==>
    member _.MaxRejected with get() = maxRejected and set(v) = maxRejected <- v; config <- {config with MaxRejected = Some v}
    ///The size to use for the first test.
    member _.StartSize with get() = startSize and set(v) = startSize <- v; config <- {config with StartSize = Some v}
    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member _.EndSize with get() = endSize and set(v) = endSize <- v; config <- {config with EndSize = Some v}
    ///Output all generated arguments.
    member _.Verbose with get() = verbose and set(v) = verbose <- v; config <- {config with Verbose = Some v}
    ///The Arbitrary instances to use for this test method. The Arbitrary instances
    ///are merged in back to front order i.e. instances for the same generated type
    ///at the front of the array will override those at the back.
    member _.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v; config <- {config with Arbitrary = v}
    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
    ///test failures.
    member _.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v; config <- {config with QuietOnSuccess = Some v}

    member internal _.Config = config

///Set common configuration for all properties within this class or module
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Assembly, AllowMultiple = false)>]
type public PropertiesAttribute() = inherit PropertyAttribute()

/// The xUnit.v3 test runner for the PropertyAttribute that executes the test via FsCheck
type PropertyTestCase =
    inherit XunitTestCase

    [<Obsolete>]
    new() = { inherit XunitTestCase() }

    new(testMethod, testCaseDisplayName, uniqueID, explicit, ?skipException, ?skipReason, ?skipType, ?skipUnless, ?skipWhen, ?traits, ?testMethodArguments, ?sourceFilePath, ?sourceLineNumber, ?timeout) =
        let skipException       = skipException       |> Option.toObj
        let skipReason          = skipReason          |> Option.toObj
        let skipType            = skipType            |> Option.toObj
        let skipUnless          = skipUnless          |> Option.toObj
        let skipWhen            = skipWhen            |> Option.toObj
        let traits              = traits              |> Option.toObj
        let testMethodArguments = testMethodArguments |> Option.toObj
        let sourceFilePath      = sourceFilePath      |> Option.toObj
        let sourceLineNumber    = sourceLineNumber    |> Option.toNullable
        let timeout             = timeout             |> Option.toNullable
        { inherit XunitTestCase(testMethod, testCaseDisplayName, uniqueID, explicit, skipException, skipReason, skipType, skipUnless, skipWhen, traits, testMethodArguments, sourceFilePath, sourceLineNumber, timeout) }

    static let combineAttributes (configs: (PropertyConfig option) list) =
        configs
        |> List.choose id
        |> List.reduce(fun higherLevelAttribute lowerLevelAttribute -> 
            PropertyConfig.combine lowerLevelAttribute higherLevelAttribute)
        
    member this.Init(output:TestOutputHelper) =
        let getPropertiesOnDeclaringClasses (testClass: IXunitTestClass) = 
            [   let mutable current: Type = testClass.Class
                while not (isNull current) do
                    yield current.GetTypeInfo().GetCustomAttributes<PropertiesAttribute>()
                          |> Seq.tryHead
                          |> Option.map (fun attr -> attr.Config)
                    current <- current.DeclaringType]
            |> List.rev
            
        let getConfig (attr: PropertyAttribute) =
            attr.Config

        let config = combineAttributes [
              yield this.TestMethod.TestClass.Class.Assembly.GetCustomAttributes<PropertiesAttribute>() |> Seq.tryHead |> Option.map getConfig
              yield! getPropertiesOnDeclaringClasses this.TestMethod.TestClass
              yield this.TestMethod.Method.GetCustomAttributes<PropertyAttribute>() |> Seq.head |> getConfig |> Some]
        
        { config with Arbitrary = config.Arbitrary }
        |> PropertyConfig.toConfig output 
        
    member this.MakeInvoker() =
        let runner =
            { new XunitTestRunner() with
                override _.InvokeTest(ctxt: XunitTestRunnerContext, instance) = 
                    let target = instance |> Option.ofObj
                    let helper = TestOutputHelper()
                    helper.Initialize(ctxt.MessageBus, ctxt.Test)
                    use _cleanup = { new IDisposable with member _.Dispose() = helper.Uninitialize() }
                    let config = this.Init(helper)
                    let xunitRunner = if config.Runner :? XunitRunner then (config.Runner :?> XunitRunner) else XunitRunner()
        
                    let sw = Stopwatch.StartNew()
                    Check.Method(config, ctxt.TestMethod, ?target=target)
                    let ts = sw.Elapsed
                    match xunitRunner.Result with
                    | TestResult.Passed _ ->
                        helper.WriteLine(Runner.onFinishedToString "" xunitRunner.Result)
                    | TestResult.Exhausted _ ->
                        raise (PropertyFailedException(xunitRunner.Result))
                    | TestResult.Failed (testdata, originalArgs, shrunkArgs, Outcome.Failed e, originalSeed, lastSeed, lastSize)  ->
                        let message = sprintf "%s%s" Environment.NewLine (Runner.onFailureToString "" testdata originalArgs shrunkArgs originalSeed lastSeed lastSize)
                        raise (PropertyFailedException(message, e))
                    | TestResult.Failed _ ->
                        raise (PropertyFailedException(xunitRunner.Result))

                    ValueTask<_>(ts)
            }
        { new XunitTestCaseRunner() with
            override _.RunTest(x, test) =
                runner.Run(test, x.MessageBus, x.ConstructorArguments, x.ExplicitOption, x.Aggregator.Clone(), x.CancellationTokenSource, x.BeforeAfterTestAttributes)
        }
        
    member this.TestExec(opts: ExplicitOption, buss: IMessageBus, ctorArgs: obj array, aggregator: ExceptionAggregator, cts: CancellationTokenSource) = async {
        let invoker = this.MakeInvoker()
        let! tests = aggregator.RunAsync(Func<_>(this.CreateTests), []).AsTask() |> Async.AwaitTask
        return! invoker.Run(this, tests, buss, aggregator.Clone(), cts, this.TestCaseDisplayName, this.SkipReason, opts, ctorArgs).AsTask() |> Async.AwaitTask
    }

    interface ISelfExecutingXunitTestCase with
        member this.Run (opts: ExplicitOption, messageBus: IMessageBus, ctorArgs: obj array, aggregator: ExceptionAggregator, cts: CancellationTokenSource) = 
            ValueTask<RunSummary>(Async.StartImmediateAsTask(this.TestExec(opts, messageBus, ctorArgs, aggregator, cts)))
         
         
/// xUnit.v3 test case discoverer to link the method with the PropertyAttribute to the PropertyTestCase
/// so the test can be run via FsCheck.
type PropertyDiscoverer(messageSink:IMessageSink) =

    new () = PropertyDiscoverer(null)

    member _.MessageSink = messageSink

    interface IXunitTestCaseDiscoverer with
        override this.Discover(discoveryOptions: ITestFrameworkDiscoveryOptions, testMethod: IXunitTestMethod, attr: IFactAttribute)=
            let result = ResizeArray<IXunitTestCase>()
            let struct (testCaseDisplayName, explicit, skipExceptions, skipReason, _, _, _, _, _, _, uniqueID, testMethod) =
                TestIntrospectionHelper.GetTestCaseDetails(discoveryOptions, testMethod, attr, null, Nullable(), null, null)
            let traits = TestIntrospectionHelper.GetTraits(testMethod, null)
            result.Add(PropertyTestCase(testMethod, testCaseDisplayName, uniqueID, explicit, skipExceptions, skipReason, traits=traits))
            ValueTask<_>(result  :> IReadOnlyCollection<IXunitTestCase>)


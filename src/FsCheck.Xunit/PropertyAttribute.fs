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
    member x.Result = result.Value
    interface IRunner with
        override x.OnStartFixture t = ()
        override x.OnArguments (ntest,args, every) =
            printf "%s" (every ntest args)
        override x.OnShrink(args, everyShrink) =
            printf "%s" (everyShrink args)
        override x.OnFinished(name,testResult) =
            result <- Some testResult

///Override Arbitrary instances for FsCheck tests within the attributed class
///or module.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type ArbitraryAttribute(types:Type[]) =
    inherit Attribute()
    new(typ:Type) = ArbitraryAttribute([|typ|])
    member x.Arbitrary = types

///Run this method as an FsCheck test.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<XunitTestCaseDiscoverer("FsCheck.Xunit.PropertyDiscoverer", "FsCheck.Xunit")>]
type public PropertyAttribute() =
    inherit FactAttribute()
    let mutable maxTest = Config.Default.MaxTest
    let mutable maxFail = Config.Default.MaxFail
    let mutable replay = Config.Default.Replay
    let mutable startSize = Config.Default.StartSize
    let mutable endSize = Config.Default.EndSize
    let mutable verbose = false
    let mutable quietOnSuccess = false
    let mutable arbitrary = Config.Default.Arbitrary |> List.toArray
    ///If set, the seed to use to start testing. Allows reproduction of previous runs. You can just paste
    ///the tuple from the output window, e.g. 12344,12312 or (123,123).
    member x.Replay with get() = match replay with None -> String.Empty | Some (Random.StdGen (x,y)) -> sprintf "%A" (x,y)
                    and set(v:string) =
                        //if someone sets this, we want it to throw if it fails
                        let split = v.Trim('(',')').Split([|","|], StringSplitOptions.RemoveEmptyEntries)
                        let elem1 = Int32.Parse(split.[0])
                        let elem2 = Int32.Parse(split.[1])
                        replay <- Some <| Random.StdGen (elem1,elem2)
    member internal x.ReplayStdGen = replay
    ///The maximum number of tests that are run.
    member x.MaxTest with get() = maxTest and set(v) = maxTest <- v
    ///The maximum number of tests where values are rejected, e.g. as the result of ==>
    member x.MaxFail with get() = maxFail and set(v) = maxFail <- v
    ///The size to use for the first test.
    member x.StartSize with get() = startSize and set(v) = startSize <- v
    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member x.EndSize with get() = endSize and set(v) = endSize <- v
    ///Output all generated arguments.
    member x.Verbose with get() = verbose and set(v) = verbose <- v
    ///The Arbitrary instances to use for this test method. The Arbitrary instances
    ///are merged in back to front order i.e. instances for the same generated type
    ///at the front of the array will override those at the back.
    member x.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v
    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
    ///test failures.
    member x.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v

/// The xUnit2 test runner for the PropertyAttribute that executes the test via FsCheck
type PropertyTestCase(diagnosticMessageSink:IMessageSink, defaultMethodDisplay:TestMethodDisplay, testMethod:ITestMethod, ?testMethodArguments:obj []) =
    inherit XunitTestCase(diagnosticMessageSink, defaultMethodDisplay, testMethod, (match testMethodArguments with | None -> null | Some v -> v))

    new() = new PropertyTestCase(null, TestMethodDisplay.ClassAndMethod, null)

    member this.Init() =
        let factAttribute = this.TestMethod.Method.GetCustomAttributes(typeof<PropertyAttribute>) |> Seq.head
        let arbitrariesOnMethod = factAttribute.GetNamedArgument "Arbitrary"
        let arbitrariesOnClass =
            this.TestMethod.TestClass.Class.GetCustomAttributes(typeof<ArbitraryAttribute>)
                |> Seq.collect (fun attr -> attr.GetNamedArgument "Arbitrary")

        let arbitraries =
            Config.Default.Arbitrary
            |> Seq.append arbitrariesOnClass
            |> Seq.append arbitrariesOnMethod
            |> Seq.toList

        { Config.Default with
                Replay = factAttribute.GetNamedArgument("ReplayStdGen")
                MaxTest = factAttribute.GetNamedArgument("MaxTest")
                MaxFail = factAttribute.GetNamedArgument("MaxFail")
                StartSize = factAttribute.GetNamedArgument("StartSize")
                EndSize = factAttribute.GetNamedArgument("EndSize")
                QuietOnSuccess = factAttribute.GetNamedArgument("QuietOnSuccess")
                Every = if factAttribute.GetNamedArgument("Verbose") then Config.Verbose.Every else Config.Quick.Every
                EveryShrink = if factAttribute.GetNamedArgument("Verbose") then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                Arbitrary = arbitraries
                Runner = new XunitRunner()
            }

    override this.RunAsync(diagnosticMessageSink:IMessageSink, messageBus:IMessageBus, constructorArguments:obj [], aggregator:ExceptionAggregator, cancellationTokenSource:Threading.CancellationTokenSource) =
        let test = new XunitTest(this, this.DisplayName)
        let summary = new RunSummary(Total = 1);

        let testExec() =
            let config = this.Init()
            let timer = ExecutionTimer()
            let result =
                try
                    let xunitRunner = if config.Runner :? XunitRunner then (config.Runner :?> XunitRunner) else new XunitRunner()
                    let runMethod = this.TestMethod.Method.ToRuntimeMethod()
                    let target =
                        constructorArguments
                            |> Array.tryFind (fun x -> x :? TestOutputHelper)
                            |> Option.iter (fun x -> (x :?> TestOutputHelper).Initialize(messageBus, test))
                        let testClass = this.TestMethod.TestClass.Class.ToRuntimeType()
                        if this.TestMethod.TestClass <> null && not this.TestMethod.Method.IsStatic then
                            Some (test.CreateTestClass(testClass, constructorArguments, messageBus, timer, cancellationTokenSource))
                        else None

                    Check.Method(config, runMethod, ?target=target)

                    match xunitRunner.Result with
                          | TestResult.True _ ->
                            let output = Runner.onFinishedToString "" xunitRunner.Result
                            new TestPassed(test, timer.Total, output) :> TestResultMessage
                          | TestResult.Exhausted testdata ->
                            summary.Failed <- summary.Failed + 1
                            upcast new TestFailed(test, timer.Total, "", new PropertyFailedException(xunitRunner.Result))
                          | TestResult.False (testdata, originalArgs, shrunkArgs, Outcome.Exception e, seed)  ->
                            let message = sprintf "%s%s" Environment.NewLine (Runner.onFailureToString "" testdata originalArgs shrunkArgs seed)
                            upcast new TestFailed(test, timer.Total, "", new PropertyFailedException(message, e))
                          | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed)  ->
                            summary.Failed <- summary.Failed + 1
                            upcast new TestFailed(test, timer.Total, "", new PropertyFailedException(xunitRunner.Result))
                with
                    | ex -> upcast new TestFailed(test, timer.Total, "Exception during test:", ex)

            messageBus.QueueMessage(result) |> ignore
            summary.Time <- summary.Time + result.ExecutionTime
            if not (messageBus.QueueMessage(new TestFinished(test, summary.Time, result.Output))) then
                cancellationTokenSource.Cancel() |> ignore
            summary

        if not (messageBus.QueueMessage(new TestStarting(test))) then
            cancellationTokenSource.Cancel() |> ignore

        if not(String.IsNullOrEmpty(this.SkipReason)) then
            summary.Skipped <- summary.Skipped + 1
            if not(messageBus.QueueMessage(new TestSkipped(test, this.SkipReason))) then
                cancellationTokenSource.Cancel() |> ignore
            Task.Factory.StartNew(fun () -> summary)
        else
            Task.Factory.StartNew<RunSummary>(fun () -> testExec())

/// xUnit2 test case discoverer to link the method with the PropertyAttribute to the PropertyTestCase
/// so the test can be run via FsCheck.
type PropertyDiscoverer(messageSink:IMessageSink) =

    new () = PropertyDiscoverer(null)

    member x.MessageSink = messageSink

    interface IXunitTestCaseDiscoverer with
        override this.Discover(discoveryOptions:ITestFrameworkDiscoveryOptions, testMethod:ITestMethod, factAttribute:IAttributeInfo)=
            let ptc = new PropertyTestCase(this.MessageSink, discoveryOptions.MethodDisplayOrDefault(), testMethod)
            Seq.singleton (ptc :> IXunitTestCase)
// note: FsCheck.Xunit.dll needs to exist at: %TEMP%\VisualStudioTestExplorerExtensions\xunit.runner.visualstudio.2.0.0\build\_common
//  as indicated here http://xunit.github.io/docs/running-tests-in-vs.html
// also the same for the console runner, why?

namespace FsCheck.Xunit

open System

open Xunit
open Xunit.Sdk
open Xunit.Abstractions
open FsCheck
open System.Threading.Tasks

type PropertyFailedException(testResult:FsCheck.TestResult) =
    inherit XunitException(sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" testResult), "sorry no stacktrace")

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

type PropertyTest (displayName, testClass) =
    interface ITest with
        member this.DisplayName = displayName
        member this.TestCase = testClass

type PropertyTestCase(diagnosticMessageSink:IMessageSink, defaultMethodDisplay:TestMethodDisplay, testMethod:ITestMethod, ?testMethodArguments:obj []) =
    inherit XunitTestCase(diagnosticMessageSink, defaultMethodDisplay, testMethod, (match testMethodArguments with | None -> null | Some v -> v))

    new() =
        new PropertyTestCase(null, TestMethodDisplay.ClassAndMethod, null)

    member this.Init() =
        let factAttribute = this.TestMethod.Method.GetCustomAttributes(typeof<PropertyAttribute>) |> Seq.head
        let arbitraryOnMethod = factAttribute.GetNamedArgument "Arbitrary"
        let arbitrariesOnClass =
            this.TestMethod.TestClass.Class.GetCustomAttributes(typeof<ArbitraryAttribute>)
                |> Seq.collect (fun attr -> attr.GetNamedArgument "Arbitrary")

        let arbitraries =
            Config.Default.Arbitrary
            |> Seq.append arbitrariesOnClass
            |> Seq.append arbitraryOnMethod
            |> Seq.toList

        {Config.Default with
                Replay = factAttribute.GetNamedArgument("ReplayStdGen")
                MaxTest = factAttribute.GetNamedArgument("MaxTest")
                MaxFail = factAttribute.GetNamedArgument("MaxFail")
                StartSize = factAttribute.GetNamedArgument("StartSize")
                EndSize = factAttribute.GetNamedArgument("EndSize")
                Every = if factAttribute.GetNamedArgument("Verbose") then Config.Verbose.Every else Config.Quick.Every
                EveryShrink = if factAttribute.GetNamedArgument("Verbose") then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                Arbitrary = arbitraries
                Runner = new XunitRunner()
            }

    override this.RunAsync(diagnosticMessageSink:IMessageSink, messageBus:IMessageBus, constructorArguments:obj [], aggregator:ExceptionAggregator, cancellationTokenSource:Threading.CancellationTokenSource) =
        let test = new PropertyTest(this.DisplayName, this)
        let summary = new RunSummary(Total = 1);
        if not (messageBus.QueueMessage(new TestStarting(test))) then
            cancellationTokenSource.Cancel() |> ignore

        if not(String.IsNullOrEmpty(this.SkipReason)) then
            summary.Skipped <- summary.Skipped + 1
            if not(messageBus.QueueMessage(new TestSkipped(test, this.SkipReason))) then
                cancellationTokenSource.Cancel() |> ignore
            Task.Factory.StartNew(fun () -> summary)
        else
            let testExec() =
                let config = this.Init()
                let sw = System.Diagnostics.Stopwatch.StartNew()
                let result =
                    try
                        let xunitRunner = if config.Runner :? XunitRunner then (config.Runner :?> XunitRunner) else new XunitRunner()
                        let runMethod = this.TestMethod.Method.ToRuntimeMethod()
                        let target =
                            if this.TestMethod.TestClass <> null && not this.TestMethod.Method.IsStatic then
                                Some (Activator.CreateInstance(this.TestMethod.TestClass.Class.ToRuntimeType()))
                            else None

                        Check.Method(config, runMethod, ?target=target)

                        let result =
                            match xunitRunner.Result with
                                  | TestResult.True _ ->
                                    (0, new TestPassed(test, (decimal)sw.Elapsed.TotalSeconds, (Runner.onFinishedToString "" xunitRunner.Result)) :> TestResultMessage)
                                  | TestResult.Exhausted testdata ->
                                    summary.Failed <- summary.Failed + 1
                                    (1, upcast new TestFailed(test, (decimal)sw.Elapsed.TotalSeconds, (sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" xunitRunner.Result)), new PropertyFailedException(xunitRunner.Result)))
                                  | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed)  ->
                                    summary.Failed <- summary.Failed + 1
                                    (1, upcast new TestFailed(test, (decimal)sw.Elapsed.TotalSeconds, (sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" xunitRunner.Result)), new PropertyFailedException(xunitRunner.Result)))
                        result
                    with
                        | ex -> (1, upcast new TestFailed(test, (decimal)sw.Elapsed.TotalSeconds, "Exception during test:", ex))

                let failed = fst result
                let testMessage = snd result
                messageBus.QueueMessage(testMessage) |> ignore
                summary.Time <- summary.Time + testMessage.ExecutionTime
                if not (messageBus.QueueMessage(new TestFinished(test, summary.Time, testMessage.Output))) then
                    cancellationTokenSource.Cancel() |> ignore
                summary

            Task.Factory.StartNew<RunSummary>(fun () -> testExec())

type PropertyDiscoverer(messageSink:IMessageSink) =

    new () = PropertyDiscoverer(null)

    member x.MessageSink : IMessageSink = messageSink

    interface IXunitTestCaseDiscoverer with
        override this.Discover(discoveryOptions:ITestFrameworkDiscoveryOptions, testMethod:ITestMethod, factAttribute:IAttributeInfo)=
            printfn "%s" testMethod.Method.Name
            let ptc = new PropertyTestCase(this.MessageSink, discoveryOptions.MethodDisplayOrDefault(), testMethod)
            Seq.singleton (ptc :> IXunitTestCase)

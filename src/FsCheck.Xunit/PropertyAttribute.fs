
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
type private XunitRunner() =
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
[<XunitTestCaseDiscoverer("FsCheck.Xunit.PropertyDiscoverer", "xunit.execution.{Platform}")>]
type PropertyAttribute() =
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
    //at the front of the array will override those at the back.
    member x.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v
    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
    ///test failures.
    member x.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v

type PropertyTestCase(diagnosticMessageSink:IMessageSink, defaultMethodDisplay:TestMethodDisplay, testMethod:ITestMethod, ?testMethodArguments:obj []) =
    inherit XunitTestCase(diagnosticMessageSink, defaultMethodDisplay, testMethod, (match testMethodArguments with | None -> null | Some v -> v))
    let mutable config = Config.Default

    member x.Config with get() = config and set(c) = config <- c

    override this.Initialize() =
        base.Initialize()
//            match xunitRunner.Result with
//            | TestResult.True _ ->
//                if not property.QuietOnSuccess then
//                    printf "%s%s" Environment.NewLine (Runner.onFinishedToString "" xunitRunner.Result)
//                upcast new PassedResult(runMethod, this.DisplayName)
//            | TestResult.Exhausted testdata ->
//                upcast new FailedResult(runMethod, PropertyFailedException(xunitRunner.Result), this.DisplayName)
//            | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed)  ->
//                upcast new FailedResult(runMethod, PropertyFailedException(xunitRunner.Result), this.DisplayName)
        ()

    override this.RunAsync(diagnosticMessageSink:IMessageSink, messageBus:IMessageBus, constructorArguments:obj [], aggregator:ExceptionAggregator, cancellationTokenSource:Threading.CancellationTokenSource) =
        let testExec() =
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let target = if testMethod.TestClass <> null then Some (testMethod.TestClass :> obj) else None
            let runMethod = testMethod.Method.ToRuntimeMethod()
            Check.Method(config, runMethod, ?target=target)
            sw.Stop()
// #WARNING IMPLEMENT THE SKIPPED SETTING?
            let skipped = 0
            let failed = match (config.Runner :?> XunitRunner).Result with
                                | TestResult.True _ ->
        //                            if not property.QuietOnSuccess then
        //                                printf "%s%s" Environment.NewLine (Runner.onFinishedToString "" xunitRunner.Result)
                                    0
                                | TestResult.Exhausted testdata ->
                                    //upcast new FailedResult(runMethod, PropertyFailedException(xunitRunner.Result), this.DisplayName)
                                    1
                                | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed)  ->
                                    //upcast new FailedResult(runMethod, PropertyFailedException(xunitRunner.Result), this.DisplayName)
                                    1
            new RunSummary(Time = (decimal sw.Elapsed.TotalSeconds), Total = 1, Failed = failed, Skipped = skipped)
        Task.Factory.StartNew<RunSummary>(fun () -> testExec())


type PropertyDiscoverer(messageSink:IMessageSink) =
    member this.MessageSink = messageSink

    interface IXunitTestCaseDiscoverer with
        /// Discover test cases from a test method.
        /// discoveryOptions: The discovery options to be used.
        /// testMethod: The test method the test cases belong to.
        /// factAttribute: The fact attribute attached to the test method.
        //override this.Discover discoveryOptions -> testMethod -> factAttribute =
        override this.Discover(discoveryOptions:ITestFrameworkDiscoveryOptions, testMethod:ITestMethod, factAttribute:IAttributeInfo)=
            let xunitRunner = XunitRunner()
            let property = factAttribute :?> PropertyAttribute
            let arbitraries =
                testMethod.TestClass.Class.BaseType
                |> Seq.unfold (fun t -> if t <> null then Some(t,t.BaseType) else None)
                // warning: fix the typeof<AA>.FullName
                |> Seq.map (fun t -> t.GetCustomAttributes(typeof<ArbitraryAttribute>.FullName))
                |> Seq.filter (fun attr -> (Seq.length attr) = 1)
                |> Seq.collect (fun attr -> ((Seq.head attr) :?> ArbitraryAttribute).Arbitrary)
                |> Seq.append Config.Default.Arbitrary
                |> Seq.toList
            let config =
                {Config.Default with
                    Replay = property.ReplayStdGen
                    MaxTest = property.MaxTest
                    MaxFail = property.MaxFail
                    StartSize = property.StartSize
                    EndSize = property.EndSize
                    Every = if property.Verbose then Config.Verbose.Every else Config.Quick.Every
                    EveryShrink = if property.Verbose then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                    Arbitrary = arbitraries
                    Runner = xunitRunner
                }
            let ptc = new PropertyTestCase(this.MessageSink, discoveryOptions.MethodDisplayOrDefault(), testMethod, Config = config)
            Seq.singleton (ptc :> IXunitTestCase)

(*
    // EnumerateTestCommands - get test commands represented by this test method.
    // ITestCommand - invokes a test method

    // IMethodInfo exists in the new one so that could be the basis of the new search
    override this.EnumerateTestCommands(methodInfo:IMethodInfo) :seq<ITestCommand> =
        { new TestCommand(methodInfo, null, 0) with
            override x.Execute(testClass:obj) : MethodResult =
                let xunitRunner = XunitRunner()
                let arbitraries =
                    methodInfo.Class.Type
                    |> Seq.unfold (fun t -> if t <> null then Some(t,t.DeclaringType) else None)
                    |> Seq.map (fun t -> t.GetCustomAttributes(typeof<ArbitraryAttribute>, true))
                    |> Seq.filter (fun attr -> attr.Length = 1)
                    |> Seq.collect (fun attr -> (attr.[0] :?> ArbitraryAttribute).Arbitrary)
                    |> Seq.append this.Arbitrary
                    |> Seq.toList
                let config =
                    {Config.Default with
                        Replay = this.ReplayStdGen
                        MaxTest = this.MaxTest
                        MaxFail = this.MaxFail
                        StartSize = this.StartSize
                        EndSize = this.EndSize
                        Every = if this.Verbose then Config.Verbose.Every else Config.Quick.Every
                        EveryShrink = if this.Verbose then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                        Arbitrary = arbitraries
                        Runner = xunitRunner
                    }
                Check.Method(config, methodInfo.MethodInfo,?target=if testClass <> null then Some testClass else None)
                match xunitRunner.Result with
                | TestResult.True _ ->
                    if not quietOnSuccess then
                        printf "%s%s" Environment.NewLine (Runner.onFinishedToString "" xunitRunner.Result)
                    upcast new PassedResult(methodInfo,this.DisplayName)
                | TestResult.Exhausted testdata ->
                    upcast new FailedResult(methodInfo,PropertyFailedException(xunitRunner.Result), this.DisplayName)
                | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed)  ->
                    upcast new FailedResult(methodInfo, PropertyFailedException(xunitRunner.Result), this.DisplayName)
        } :> ITestCommand
        |> Seq.singleton
*)

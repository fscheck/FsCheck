﻿namespace FsCheck.NUnit

open System
open System.Threading

open FsCheck

open NUnit.Framework
open NUnit.Framework.Interfaces
open NUnit.Framework.Internal

///help consumers remove the unneeded classes
[<Obsolete("This class is no longer needed for running NUnit v3.", true)>]
type FsCheckTestCaseBuilder() =
    do
        failwith "This class is no longer needed for running NUnit v3."

///help consumers remove the unneeded classes
[<Obsolete("This class is no longer needed for running NUnit v3.", true)>]
type FsCheckAddin() =
    do
        failwith "This class is no longer needed for running NUnit v3."

//can not be an anonymous type because of let mutable.
type private NunitRunner() =
    let mutable result = None
    member __.Result = result.Value
    interface IRunner with
        override __.OnStartFixture _ = ()
        override __.OnArguments (ntest,args, every) =
            let argsForOutput = every ntest args
            // Only write the args if there's something to write.
            // Although it may seem harmless to write an empty string, writing anything at this stage seems to
            // trigger NUnit's formatting system, so that it adds a 'header' for the test in the output. That
            // doesn't look pretty in the cases where it turns out that there's truly nothing to write (e.g.
            // when QuietOnSuccess is true, and the Property passed.
            if not (String.IsNullOrWhiteSpace argsForOutput) then
                printfn "%s" argsForOutput
        override __.OnShrink(args, everyShrink) =
            printfn "%s" (everyShrink args)
        override __.OnFinished(_,testResult) =
            result <- Some testResult

///Run this method as an FsCheck test.
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type PropertyAttribute() =
    inherit TestAttribute()

    let mutable maxTest = Config.Default.MaxTest
    let mutable maxFail = Config.Default.MaxFail
    let mutable startSize = Config.Default.StartSize
    let mutable endSize = Config.Default.EndSize
    let mutable verbose = false
    let mutable quietOnSuccess = false
    let mutable replay = null
    let mutable arbitrary = Config.Default.Arbitrary |> List.toArray

    ///If set, the seed to use to start testing. Allows reproduction of previous runs. You can just paste
    ///the tuple from the output window, e.g. 12344,12312 or (123,123).
    member __.Replay with get() = replay and set(v) = replay <- v
    ///The maximum number of tests that are run.
    member __.MaxTest with get() = maxTest and set(v) = maxTest <- v
    ///The maximum number of tests where values are rejected, e.g. as the result of ==>
    member __.MaxFail with get() = maxFail and set(v) = maxFail <- v
    ///The size to use for the first test.
    member __.StartSize with get() = startSize and set(v) = startSize <- v
    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member __.EndSize with get() = endSize and set(v) = endSize <- v
    ///Output all generated arguments.
    member __.Verbose with get() = verbose and set(v) = verbose <- v
    ///The Arbitrary instances to use for this test method. The Arbitrary instances
    ///are merged in back to front order i.e. instances for the same generated type
    ///at the front of the array will override those at the back.
    member __.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v
    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
    ///test failures.
    member __.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v

    interface ISimpleTestBuilder with
        override __.BuildFrom(mi, suite) =
            FsCheckTestMethod(mi, suite) :> TestMethod

    interface IWrapTestMethod with
        override __.Wrap command:Internal.Commands.TestCommand =
            {new Internal.Commands.TestCommand(command.Test) with
                override __.Execute context = match command.Test with
                                              | :? FsCheckTestMethod as testMethod -> testMethod.RunTest(context)
                                              | _ -> command.Execute(context) }

and FsCheckTestMethod(mi : IMethodInfo, parentSuite : Test) =
    inherit TestMethod(mi, parentSuite)

    member x.RunTest context =
        let testResult = x.MakeTestResult()
        TestExecutionContext.CurrentContext.CurrentResult <- testResult
        try
            try
                x.RunSetUp()
                x.RunTestCase context testResult
            with
                | ex -> x.HandleException ex testResult FailureSite.SetUp
        finally
            x.RunTearDown testResult
        testResult

    member private x.RunSetUp() =
        if x.SetUpMethods <> null then
            x.SetUpMethods |> Array.iter x.InvokeMethodIgnore

    member private x.RunTearDown testResult =
        try
            if x.TearDownMethods <> null then
                x.TearDownMethods
                |> Array.rev
                |> Array.iter x.InvokeMethodIgnore
        with
            | ex ->
                testResult.RecordTearDownException(x.FilterException ex)

    member private x.InvokeMethodIgnore mi =
        Reflect.InvokeMethod(mi, if mi.IsStatic then null else x.Fixture) |> ignore

    member private __.FilterException ex =
        match ex with
        | :? NUnitException as nue when nue.InnerException <> null -> nue.InnerException
        | _ -> ex

    member private x.RunTestCase context testResult =
        try
            x.RunTestMethod context testResult
        with
            | ex -> x.HandleException ex testResult FailureSite.Test

    member private x.GetFsCheckPropertyAttribute() =
        x.Method.GetCustomAttributes<PropertyAttribute> false
        |> Seq.head 

    member private x.HandleException ex testResult failureSite =
        match ex with
#if NETSTANDARD1_6
#else
        | :? ThreadAbortException -> Thread.ResetAbort()
#endif
        | _ -> ()
        testResult.RecordException(x.FilterException <| ex, failureSite)

    member private x.RunTestMethod context testResult =
        let parseStdGen (str: string) =
            //if someone sets this, we want it to throw if it fails
            let split = str.Trim('(',')').Split([|","|], StringSplitOptions.RemoveEmptyEntries)
            let elem1 = Int32.Parse(split.[0])
            let elem2 = Int32.Parse(split.[1])
            Random.StdGen (elem1,elem2)
        let attr = x.GetFsCheckPropertyAttribute()
        let testRunner = NunitRunner()
        let config = { Config.Default with
                        MaxTest = attr.MaxTest
                        MaxFail = attr.MaxFail
                        StartSize = attr.StartSize
                        EndSize = attr.EndSize
                        Every = if attr.Verbose then Config.Verbose.Every else Config.Quick.Every
                        EveryShrink = if attr.Verbose then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                        Arbitrary = attr.Arbitrary |> Array.toList
                        Replay = match attr.Replay with
                                    | null -> Config.Default.Replay
                                    | s -> parseStdGen s |> Some
                        Runner = testRunner }

        let target = if x.Fixture <> null then Some x.Fixture
                     elif x.Method.MethodInfo.IsStatic then None
                     else Some context.TestObject
        Check.Method(config, x.Method.MethodInfo, ?target = target)
        match testRunner.Result with
        | TestResult.True _ ->
            if not attr.QuietOnSuccess then
                printfn "%s" (Runner.onFinishedToString "" testRunner.Result)
            testResult.SetResult(ResultState(TestStatus.Passed))
        | TestResult.Exhausted _ ->
            let msg = sprintf "Exhausted: %s" (Runner.onFinishedToString "" testRunner.Result)
            testResult.SetResult(new ResultState(TestStatus.Failed, msg), msg)
        | TestResult.False (testdata, originalArgs, shrunkArgs, Outcome.Exception e, seed)  ->
            let msg = sprintf "%s" (Runner.onFailureToString "" testdata originalArgs shrunkArgs seed)
            testResult.SetResult(new ResultState(TestStatus.Failed, msg), msg)
        | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed) ->
            let msg = sprintf "%s" (Runner.onFinishedToString "" testRunner.Result)
            testResult.SetResult(new ResultState(TestStatus.Failed, msg), msg)
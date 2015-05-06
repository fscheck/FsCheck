namespace FsCheck.NUnit

open System
open System.Reflection
open System.Threading

open FsCheck

open NUnit.Framework
open NUnit.Framework.Interfaces
open NUnit.Framework.Internal

//warning  ..\nunit\bin\Debug\nunit-console.exe examples\FsCheck.NUnit.CSharpExamples\bin\Debug\FsCheck.NUnit.CSharpExamples.dll --trace=Verbose --pause

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
    let mutable arbitrary = Config.Default.Arbitrary |> List.toArray

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

//    interface ISimpleTestBuilder with
//        override x.BuildFrom (mi:Reflection.MethodInfo, suite:Internal.Test) =
//            FsCheckTestMethod(mi) :> TestMethod

    interface IWrapTestMethod with
        override x.Wrap command:Internal.Commands.TestCommand =
            {new Internal.Commands.TestCommand(command.Test) with
                override x.Execute context = FsCheckTestMethod(command.Test.Method).RunTest() }


and FsCheckTestMethod(mi : MethodInfo) =
    inherit TestMethod(mi)

    member x.RunTest() =
        let testResult = x.MakeTestResult()
        TestExecutionContext.CurrentContext.CurrentResult <- testResult
        try
            try
                x.runSetUp()
                x.runTestCase testResult
            with
                | ex -> x.handleException ex testResult FailureSite.SetUp
        finally
            x.runTearDown testResult
        testResult

    member private x.runSetUp() =
        if x.setUpMethods <> null then
            x.setUpMethods |> Array.iter x.invokeMethodIgnore

    member private x.runTearDown testResult =
        try
            if x.tearDownMethods <> null then
                x.tearDownMethods
                |> Array.rev
                |> Array.iter x.invokeMethodIgnore
        with
            | ex ->
                let tmpEx =
                    match ex with
                    | :? NUnitException -> ex.InnerException
                    | _ -> ex
                testResult.RecordTearDownException(tmpEx)

    member private x.invokeMethodIgnore mi =
        x.invokeMethod mi |> ignore

    member private x.invokeMethod (mi:MethodInfo) =
        Reflect.InvokeMethod(mi, if mi.IsStatic then null else x.Fixture)

    member private x.runTestCase testResult =
        try
            x.runTestMethod testResult
        with
            | ex -> x.handleException ex testResult FailureSite.Test

    member private x.getFsCheckPropertyAttribute() =
        let attr = x.Method.GetCustomAttributes(typeof<PropertyAttribute>, false) |> Seq.head
        attr :?> PropertyAttribute

    member private x.runTestMethod testResult =
        let attr = x.getFsCheckPropertyAttribute()
        let testRunner = { new IRunner with
                member x.OnStartFixture t = ()
                member x.OnArguments (ntest, args, every) =
                    let argsForOutput = every ntest args
                    // Only write the args if there's something to write.
                    // Although it may seem harmless to write an empty string, writing anything at this stage seems to
                    // trigger NUnit's formatting system, so that it adds a 'header' for the test in the output. That
                    // doesn't look pretty in the cases where it turns out that there's truly nothing to write (e.g.
                    // when QuietOnSuccess is true, and the Property passed.
                    if not (String.IsNullOrWhiteSpace argsForOutput) then
                        Console.Write(argsForOutput)
                member x.OnShrink(args, everyShrink) = Console.Write(everyShrink args)
                member x.OnFinished(name, fsCheckResult) =
                    let message = Runner.onFinishedToString name fsCheckResult
                    match fsCheckResult with
                    | TestResult.True _ ->
                        if not attr.QuietOnSuccess then
                            Console.WriteLine(message)
                    | _ ->
                        Console.WriteLine(message)
                        Assert.Fail(message) //TODO: find better way to tell NUnit about error. Now AssertionException is wrapped into TargetInvocationException

            }
        let config = { Config.Default with
                        MaxTest = attr.MaxTest
                        MaxFail = attr.MaxFail
                        StartSize = attr.StartSize
                        EndSize = attr.EndSize
                        Every = if attr.Verbose then Config.Verbose.Every else Config.Quick.Every
                        EveryShrink = if attr.Verbose then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                        Arbitrary = attr.Arbitrary |> Array.toList
                        Runner = testRunner }

        Check.Method(config, x.Method, ?target = if x.Fixture <> null then Some x.Fixture else None)
        testResult.SetResult(ResultState(TestStatus.Passed))

    member private x.handleException ex testResult failureSite =
        match ex with
        | :? ThreadAbortException -> Thread.ResetAbort()
        | _ -> ()
        testResult.RecordException(ex, failureSite)

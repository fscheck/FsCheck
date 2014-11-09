namespace FsCheck.NUnit.Addin

open System
open System.Reflection
open System.Threading

open NUnit.Core
open NUnit.Core.Extensibility
open NUnit.Framework

open FsCheck
open FsCheck.NUnit
open FsCheck.Fluent

type FsCheckTestMethod(mi : MethodInfo) =
    inherit TestMethod(mi)
    
    override x.RunTest() = 
        let testResult = TestResult(x)
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
                x.RecordException(tmpEx, testResult, FailureSite.TearDown)
            
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

        if x.Method.ReturnType = typeof<UnbrowsableObject> then        
            let spec = x.invokeMethod x.Method :?> UnbrowsableObject
            Check.One(config, spec.Build())
        else
            Check.Method(config, x.Method, ?target = if x.Fixture <> null then Some x.Fixture else None)        
        testResult.Success()
    
    member private x.handleException ex testResult failureSite =
        match ex with 
        | :? ThreadAbortException -> Thread.ResetAbort()
        | _ -> ()
        x.RecordException(ex, testResult, failureSite)
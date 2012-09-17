namespace FsCheck.NUnit.Addin

open System
open System.Reflection
open System.Threading
open FsCheck
open FsCheck.Fluent
open FsCheck.NUnit
open NUnit.Core
open NUnit.Core.Extensibility
open NUnit.Framework

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
        let attr = x.Method.GetCustomAttributes(typeof<FsCheckPropertyAttribute>, false) |> Seq.head
        attr :?> FsCheckPropertyAttribute
        
    member private x.runTestMethod testResult =
        let testRunner = { new IRunner with
                member x.OnStartFixture t = ()
                member x.OnArguments (ntest, args, every) = Console.Write(every ntest args)
                member x.OnShrink(args, everyShrink) = Console.Write(everyShrink args)
                member x.OnFinished(name, fsCheckResult) = 
                    let message = Runner.onFinishedToString name fsCheckResult
                    Console.WriteLine(message)
                    match fsCheckResult with
                    | TestResult.True _ -> ()
                    | _ -> Assert.Fail(message) //TODO: find better way to tell NUnit about error. Now AssertionException is wrapped into TargetInvocationException
                        
            }
        let attr = x.getFsCheckPropertyAttribute()
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

[<NUnitAddin(Description = "FsCheck addin")>]
type FsCheckAddin() =        
    interface IAddin with
        override x.Install host = 
            host.GetExtensionPoint("TestCaseBuilders").Install(x)
            true

    interface ITestCaseBuilder with
        override x.CanBuildFrom mi = 
            Reflect.HasAttribute(mi, typeof<FsCheckPropertyAttribute>.FullName, false)
        override x.BuildFrom mi =                         
            FsCheckTestMethod(mi) :> Test



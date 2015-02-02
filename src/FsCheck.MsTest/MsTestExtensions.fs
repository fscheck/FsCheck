namespace FsCheck

open System
open System.Runtime.CompilerServices
open FsCheck
open FsCheck.Fluent
open Microsoft.VisualStudio.TestTools.UnitTesting 

[<Extension>]
type MsTestExtensions =
    static member private msTestRunner() = 
            { new IRunner with
                member x.OnStartFixture t = ()
                member x.OnArguments (ntest, args, every) = Console.Write(every ntest args)
                member x.OnShrink(args, everyShrink) = Console.Write(everyShrink args)
                member x.OnFinished(name, testResult) = 
                    let message = Runner.onFinishedToString name testResult
                    match testResult with
                    | TestResult.True _ -> Console.WriteLine(message)                    
                    | _ -> Assert.Fail(message)
            }

    static member private msTestConfig() = { Config.Default with Runner = MsTestExtensions.msTestRunner() }
    static member private msTestConfigVerbose() = { Config.Verbose with Runner = MsTestExtensions.msTestRunner() }

    [<Extension>]
    static member Assert (spec : Specification) =
        Check.One(MsTestExtensions.msTestConfig(), spec.Build())

    [<Extension>]
    static member AssertVerbose (spec : Specification) =
        Check.One(MsTestExtensions.msTestConfigVerbose(), spec.Build())
    
    [<Extension>]
    static member Assert (spec : Commands.ISpecification<'a, 'b>) =
        Check.One(MsTestExtensions.msTestConfig(), Commands.asProperty spec)

    [<Extension>]
    static member AssertVerbose (spec : Commands.ISpecification<'a, 'b>) =        
        Check.One(MsTestExtensions.msTestConfigVerbose(), Commands.asProperty spec)





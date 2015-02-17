namespace FsCheck

open System
open System.Runtime.CompilerServices
open FsCheck
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
    static member Assert (spec : ICommandGenerator<'a, 'b>) =
        Check.One(MsTestExtensions.msTestConfig(), Command.asProperty spec)

    [<Extension>]
    static member AssertVerbose (spec : ICommandGenerator<'a, 'b>) =        
        Check.One(MsTestExtensions.msTestConfigVerbose(), Command.asProperty spec)





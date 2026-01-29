namespace FsCheck.Xunit

open System
open System.Runtime.CompilerServices
open FsCheck
open Xunit.Abstractions

module private Helper =
    let private runner (testOutputHelper: ITestOutputHelper) =
        { new IRunner with
            member __.OnStartFixture t =
                Runner.onStartFixtureToString t |> Helpers.safeWriteLine testOutputHelper
            member __.OnArguments (ntest,args, every) =
                every ntest args |> Helpers.safeWriteLine testOutputHelper
            member __.OnShrink(args, everyShrink) =
                everyShrink args |> Helpers.safeWriteLine testOutputHelper
            member __.OnFinished(name,testResult) = 
                Runner.onFinishedToString name testResult |> Helpers.safeWriteLine testOutputHelper
        }

    let private throwingRunner (testOutputHelper: ITestOutputHelper) =
        { new IRunner with
            member __.OnStartFixture t =
                Helpers.safeWriteLine testOutputHelper (Runner.onStartFixtureToString t)
            member __.OnArguments (ntest,args, every) =
                Helpers.safeWriteLine testOutputHelper (every ntest args)
            member __.OnShrink(args, everyShrink) =
                Helpers.safeWriteLine testOutputHelper (everyShrink args)
            member __.OnFinished(name,testResult) = 
                match testResult with
                | TestResult.Passed _ -> Helpers.safeWriteLine testOutputHelper (Runner.onFinishedToString name testResult)
                | _ -> failwithf "%s" (Runner.onFinishedToString name testResult)
        }

    let writeToXunit (config:Config) (testOutputHelper: ITestOutputHelper) =
        config.WithRunner(runner testOutputHelper)

    let writeToXunitThrow (config:Config) (testOutputHelper: ITestOutputHelper) =
        config.WithRunner(throwingRunner testOutputHelper)

[<AbstractClass;Sealed;Extension>]
type CheckExtensions =
    [<Extension>]
    static member QuickCheck(property:Property, testOutputHelper: ITestOutputHelper) =
        Check.One(Helper.writeToXunit Config.Quick testOutputHelper,property)
    [<Extension>]
    static member QuickCheck(property:Property, testName:string, testOutputHelper: ITestOutputHelper) =
        Check.One(testName,Helper.writeToXunit Config.Quick testOutputHelper,property)
    [<Extension>]
    static member QuickCheckThrowOnFailure(property:Property, testOutputHelper: ITestOutputHelper) =
        Check.One(Helper.writeToXunitThrow Config.QuickThrowOnFailure testOutputHelper,property)
    [<Extension>]
    static member VerboseCheck(property:Property, testOutputHelper: ITestOutputHelper) =
        Check.One(Helper.writeToXunit Config.Verbose testOutputHelper, property)
    [<Extension>]
    static member VerboseCheck(property:Property, testName:string, testOutputHelper: ITestOutputHelper) =
        Check.One(testName, Helper.writeToXunit Config.Verbose testOutputHelper, property)
    [<Extension>]
    static member VerboseCheckThrowOnFailure(property:Property, testOutputHelper: ITestOutputHelper) =
        Check.One(Helper.writeToXunitThrow Config.VerboseThrowOnFailure testOutputHelper,property)
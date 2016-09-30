namespace FsCheck.Xunit

open System
open System.Runtime.CompilerServices
open FsCheck
open Xunit.Abstractions

module private Helper =
    let private runner (testOutputHelper: ITestOutputHelper) =
        { new IRunner with
            member __.OnStartFixture t =
                Runner.onStartFixtureToString t |> testOutputHelper.WriteLine
            member __.OnArguments (ntest,args, every) =
                every ntest args |> testOutputHelper.WriteLine
            member __.OnShrink(args, everyShrink) =
                everyShrink args |> testOutputHelper.WriteLine
            member __.OnFinished(name,testResult) = 
                Runner.onFinishedToString name testResult |> testOutputHelper.WriteLine
        }

    let writeToXunit config (testOutputHelper: ITestOutputHelper) =
        { config with Runner = runner testOutputHelper }

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
        Check.One(Helper.writeToXunit Config.QuickThrowOnFailure testOutputHelper,property)
    [<Extension>]
    static member VerboseCheck(property:Property, testOutputHelper: ITestOutputHelper) =
        Check.One(Helper.writeToXunit Config.Verbose testOutputHelper, property)
    [<Extension>]
    static member VerboseCheck(property:Property, testName:string, testOutputHelper: ITestOutputHelper) =
        Check.One(testName, Helper.writeToXunit Config.Verbose testOutputHelper, property)
    [<Extension>]
    static member VerboseCheckThrowOnFailure(property:Property, testOutputHelper: ITestOutputHelper) =
        Check.One(Helper.writeToXunit Config.VerboseThrowOnFailure testOutputHelper,property)
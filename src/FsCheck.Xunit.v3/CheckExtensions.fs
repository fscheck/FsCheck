namespace FsCheck.Xunit

open System
open System.Runtime.CompilerServices
open Xunit
open Xunit.v3

open FsCheck

module private Helper =
    let private runner (testOutputHelper: ITestOutputHelper) =
        { new IRunner with
            member _.OnStartFixture t =
                Runner.onStartFixtureToString t |> testOutputHelper.WriteLine
            member _.OnArguments (ntest,args, every) =
                every ntest args |> testOutputHelper.WriteLine
            member _.OnShrink(args, everyShrink) =
                everyShrink args |> testOutputHelper.WriteLine
            member _.OnFinished(name,testResult) = 
                Runner.onFinishedToString name testResult |> testOutputHelper.WriteLine
        }

    let private throwingRunner (testOutputHelper: ITestOutputHelper) =
        { new IRunner with
            member _.OnStartFixture t =
                testOutputHelper.WriteLine (Runner.onStartFixtureToString t)
            member _.OnArguments (ntest,args, every) =
                testOutputHelper.WriteLine (every ntest args)
            member _.OnShrink(args, everyShrink) =
                testOutputHelper.WriteLine (everyShrink args)
            member _.OnFinished(name,testResult) = 
                match testResult with
                | TestResult.Passed _ -> testOutputHelper.WriteLine (Runner.onFinishedToString name testResult)
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
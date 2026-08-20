namespace FsCheck.Xunit

open FsCheck
open System
   
/// A runner for FsCheck (i.e. that you can use as Config.Runner) which outputs
/// to Xunit's given ITestOutputHelper.
/// For example, { Config.QuickThrowOnFailure with Runner = TestOutputRunner(output) }
type TestOutputRunner(output: Xunit.Abstractions.ITestOutputHelper) =
    interface IRunner with
        member _.OnStartFixture t =
            Helpers.safeWriteLine output (Runner.onStartFixtureToString t)
        member _.OnArguments (ntest, args, every) =
            Helpers.safeWriteLine output (every ntest args)
        member _.OnShrink(args, everyShrink) =
            Helpers.safeWriteLine output (everyShrink args)
        member _.OnFinished(name,testResult) =
            let resultText = Runner.onFinishedToString name testResult
            match testResult with
            | TestResult.Passed _ -> resultText |> Helpers.safeWriteLine output
            | _ -> failwithf "%s" resultText
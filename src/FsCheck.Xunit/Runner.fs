namespace FsCheck.Xunit

open FsCheck
   
/// A runner for FsCheck (i.e. that you can use as Config.Runner) which outputs
/// to Xunit's given ITestOutputHelper.
/// For example, { Config.QuickThrowOnFailure with Runner = TestOutputRunner(output) }
type TestOutputRunner(output: Xunit.Abstractions.ITestOutputHelper) =
    interface IRunner with
        member _.OnStartFixture t =
            output.WriteLine (Runner.onStartFixtureToString t)
        member _.OnArguments (ntest, args, every) =
            output.WriteLine (every ntest args)
        member _.OnShrink(args, everyShrink) =
            output.WriteLine (everyShrink args)
        member _.OnFinished(name,testResult) =
            let resultText = Runner.onFinishedToString name testResult
            match testResult with
            | TestResult.Passed _ -> resultText |> output.WriteLine
            | _ -> failwithf "%s" resultText
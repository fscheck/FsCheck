namespace FsCheck.Xunit

open FsCheck
open System
   
/// A runner for FsCheck (i.e. that you can use as Config.Runner) which outputs
/// to Xunit's given ITestOutputHelper.
/// For example, { Config.QuickThrowOnFailure with Runner = TestOutputRunner(output) }
type TestOutputRunner(output: Xunit.Abstractions.ITestOutputHelper) =
    // Helper to safely write to output, handling case where test may have completed
    let safeWriteLine (message: string) =
        try
            output.WriteLine(message)
        with
        | :? InvalidOperationException -> 
            // Test has completed, TestOutputHelper is no longer active
            // Silently ignore as this is expected when runner outlives test lifetime
            ()
    
    interface IRunner with
        member _.OnStartFixture t =
            safeWriteLine (Runner.onStartFixtureToString t)
        member _.OnArguments (ntest, args, every) =
            safeWriteLine (every ntest args)
        member _.OnShrink(args, everyShrink) =
            safeWriteLine (everyShrink args)
        member _.OnFinished(name,testResult) =
            let resultText = Runner.onFinishedToString name testResult
            match testResult with
            | TestResult.Passed _ -> resultText |> safeWriteLine
            | _ -> failwithf "%s" resultText
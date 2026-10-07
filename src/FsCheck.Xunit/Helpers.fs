namespace FsCheck.Xunit

open System
open Xunit.Abstractions

module internal Helpers =
    /// <summary>
    /// Safely writes to a TestOutputHelper, handling cases where the test may have completed
    /// and the helper is no longer active. This prevents InvalidOperationException when
    /// closures that capture the TestOutputHelper are called after the test lifetime ends.
    /// </summary>
    let safeWriteLine (output: ITestOutputHelper) (message: string) =
        try
            output.WriteLine(message)
        with
        | :? InvalidOperationException -> 
            // Test has completed, TestOutputHelper is no longer active
            // Silently ignore as this is expected when closures outlive test lifetime
            ()

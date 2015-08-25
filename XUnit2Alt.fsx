/// Contains tasks to run [xUnit](https://github.com/xunit/xunit) v2 unit tests.
module Fake.Testing.XUnit2Alt

open System
open System.IO
open System.Text
open Fake

(*
xUnit.net console test runner (64-bit .NET 4.0.30319.34209)
Copyright (C) 2014 Outercurve Foundation.

usage: xunit.console <assemblyFile> [configFile] [options]

Valid options:
  -parallel option       : set parallelization based on option
                         :   none - turn off all parallelization
                         :   collections - only parallelize collections
                         :   assemblies - only parallelize assemblies
                         :   all - parallelize assemblies & collections
  -maxthreads count      : maximum thread count for collection parallelization
                         :   0 - run with unbounded thread count
                         :   >0 - limit task thread pool size to 'count'
  -silent                : do not output running test count
  -noshadow              : do not shadow copy assemblies
  -teamcity              : forces TeamCity mode (normally auto-detected)
  -appveyor              : forces AppVeyor CI mode (normally auto-detected)
  -wait                  : wait for input after completion
  -trait "name=value"    : only run tests with matching name/value traits
                         : if specified more than once, acts as an OR operation
  -notrait "name=value"  : do not run tests with matching name/value traits
                         : if specified more than once, acts as an AND operation
  -xml <filename>        : output results to xUnit.net v2 style XML file
  -xmlv1 <filename>      : output results to xUnit.net v1 style XML file
  -nunit <filename>      : output results to NUnit-style XML file
  -html <filename>       : output results to HTML file
*)

/// The parallelization mode of the xUnit2 runner.
type ParallelMode =
    /// Turn off all parallelization.
    | NoParallelization
    /// Only parallelize collections.
    | Collections
    /// Only parallelize assemblies.
    | Assemblies
    /// Parallelize assemblies and collections.
    | All
    static member internal ToArgument = function
        | NoParallelization -> "none"
        | Collections -> "collections"
        | Assemblies -> "assemblies"
        | All -> "all"

/// The collection concurrency mode used by the xUnit2 runner.
type CollectionConcurrencyMode =
    /// Uses the default concurrency mode for collections.
    | Default
    /// Does not limit the number of concurrently executing collections.
    | Unlimited
    /// Limits the number of concurrently executing collections to `count`.
    | MaxThreads of count : int
    static member internal ToArgument = function
        | Default -> None
        | Unlimited -> Some 0
        | MaxThreads count -> Some count

/// The xUnit2 parameter type.
type XUnit2ArgParams =
    { /// The path to the xUnit console runner: `xunit.console.exe`
      ToolPath : string
      /// The xUnit parallelization mode.
      Parallel : ParallelMode
      /// The xUnit thread limiting strategy.
      MaxThreads : CollectionConcurrencyMode
      /// The output path of the xUnit HTML report.
      HtmlOutputPath : string option
      /// The output path of the xUnit XML report.
      XmlOutputPath : string option
      /// The output path of the xUnit XML report (in the xUnit v1 style).
      XmlV1OutputPath : string option
      /// The output path of the NUnit XML report.
      NUnitXmlOutputPath : string option
      /// The working directory for running the xunit console runner.
      WorkingDir : string option
      /// Run xUnit with shadow copy enabled.
      ShadowCopy : bool
      /// Run xUnit without reporting test progress.
      Silent : bool
      /// Maximum time to allow xUnit to run before being killed.
      TimeOut : TimeSpan
      /// Test runner error level.
      ErrorLevel : TestRunnerErrorLevel
      /// List of traits to include.
      IncludeTraits : (string * string) list
      /// List of traits to exclude.
      ExcludeTraits : (string * string) list
      /// Forces TeamCity mode (normally auto-detected).
      ForceTeamCity : bool
      /// Forces AppVeyor CI mode (normally auto-detected).
      ForceAppVeyor : bool
      /// Waits for input after completion.
      Wait : bool
      /// any additional args
      AddArg : string option }

/// The xUnit2 default parameters.
///
/// ## Defaults
///
/// - `Parallel` - `NoParallelization`
/// - `MaxThreads` - `Default`
/// - `HtmlOutputPath` - `None`
/// - `XmlOutputPath` - `None`
/// - `XmlV1OutputPath` - `None`
/// - `IncludeTraits` - `[]`
/// - `ExcludeTraits` - `[]`
/// - `ShadowCopy` - `true`
/// - `ErrorLevel` - `Error`
/// - `ToolPath` - The `xunit.console.exe` path if it exists in a subdirectory of the current directory.
/// - `WorkingDir` - `None`
/// - `TimeOut` - 5 minutes
/// - `ForceTeamCity` - `false`
/// - `ForceAppVeyor` - `false`
/// - `Silent` - `false`
/// - `Wait` - `false`
/// - `AddArg` - `None`
let XUnit2AltDefaults =
    { Parallel = NoParallelization
      MaxThreads = Default
      HtmlOutputPath = None
      XmlOutputPath = None
      XmlV1OutputPath = None
      NUnitXmlOutputPath = None
      IncludeTraits = []
      ExcludeTraits = []
      ShadowCopy = true
      ErrorLevel = Error
      ToolPath = findToolInSubPath "xunit.console.exe" (currentDirectory @@ "tools" @@ "xUnit")
      WorkingDir = None
      TimeOut = TimeSpan.FromMinutes 5.
      ForceTeamCity = false
      ForceAppVeyor = false
      Silent = false
      Wait = false
      AddArg = None }

let internal buildXUnit2Args assemblies (parameters:XUnit2ArgParams) =
    let formatTrait traitFlag (name, value) =
        sprintf @"%s ""%s=%s""" traitFlag name value
    let appendTraits traitsList traitFlag sb =
        traitsList |>
        Seq.fold (fun sb traitPair -> sb |> appendWithoutQuotes (formatTrait traitFlag traitPair)) sb

    new StringBuilder()
    |> appendFileNamesIfNotNull assemblies
    |> appendWithoutQuotes "-parallel"
    |> appendWithoutQuotes (ParallelMode.ToArgument parameters.Parallel)
    |> appendIfSome (CollectionConcurrencyMode.ToArgument parameters.MaxThreads) (sprintf "-maxthreads %d")
    |> appendIfTrueWithoutQuotes (not parameters.ShadowCopy) "-noshadow"
    |> appendIfTrueWithoutQuotes parameters.ForceTeamCity "-teamcity"
    |> appendIfTrueWithoutQuotes parameters.ForceAppVeyor "-appveyor"
    |> appendIfTrueWithoutQuotes parameters.Wait "-wait"
    |> appendIfTrueWithoutQuotes parameters.Silent "-silent"
    |> appendIfSome parameters.XmlOutputPath (sprintf @"-xml ""%s""")
    |> appendIfSome parameters.XmlV1OutputPath (sprintf @"-xmlv1 ""%s""")
    |> appendIfSome parameters.NUnitXmlOutputPath (sprintf @"-nunit ""%s""")
    |> appendIfSome parameters.HtmlOutputPath (sprintf @"-html ""%s""")
    |> appendIfSome parameters.AddArg (sprintf "%s")
    |> appendTraits parameters.IncludeTraits "-trait"
    |> appendTraits parameters.ExcludeTraits "-notrait"
    |> toText

module internal ResultHandling =
    let (|OK|Failure|) = function
        | 0 -> OK
        | x -> Failure x

    let buildErrorMessage = function
        | OK -> None
        | Failure errorCode ->
            Some (sprintf "xUnit2 reported an error (Error Code %d)" errorCode)

    let failBuildWithMessage = function
        | DontFailBuild -> traceImportant
        | _ -> failwith

    let failBuildIfXUnitReportedError errorLevel =
        buildErrorMessage
        >> Option.iter (failBuildWithMessage errorLevel)

/// Runs xUnit v2 unit tests in the given assemblies via the given xUnit2 runner.
/// Will fail if the runner terminates with non-zero exit code.
///
/// The xUnit2 runner terminates with a non-zero exit code if any of the tests
/// in the given assembly fail.
///
/// ## Parameters
///
///  - `setParams` - Function used to manipulate the default `XUnit2Params` value.
///  - `assemblies` - Sequence of one or more assemblies containing xUnit unit tests.
///
/// ## Sample usage
///
///     Target "Test" (fun _ ->
///         !! (testDir @@ "xUnit.Test.*.dll")
///           |> xUnit2Alt (fun p -> {p with HtmlOutputPath = (testDir @@ "xunit.html")})
///     )
let xUnit2 setParams assemblies =
    let details = separated ", " assemblies
    traceStartTask "xUnit2" details
    let parameters = setParams XUnit2AltDefaults

    let result =
        ExecProcess (fun info ->
            info.FileName <- parameters.ToolPath
            info.WorkingDirectory <- defaultArg parameters.WorkingDir "."
            info.Arguments <- parameters |> buildXUnit2Args assemblies) parameters.TimeOut

    ResultHandling.failBuildIfXUnitReportedError parameters.ErrorLevel result

    traceEndTask "xUnit2" details

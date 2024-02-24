#r "nuget:Fake.Core.ReleaseNotes"

open System
open System.Diagnostics
open System.IO

[<AutoOpen>]
module Utils =
    let cleanDirectories (relativeDirNames : string list) : unit =
        relativeDirNames
        |> List.iter (fun dirName ->
            let di = DirectoryInfo dirName
            try
                di.Delete true
            with :? DirectoryNotFoundException -> ()
        )

    let runProcess (command : string) (args : string seq) =
        let psi = ProcessStartInfo (command, args)
        use proc = new Process ()
        proc.StartInfo <- psi
        if not (proc.Start ()) then
            let args = args |> String.concat " "
            failwith $"Failed to start process '%s{command}' with args: %s{args}"
        proc.WaitForExit ()
        if proc.ExitCode <> 0 then
            let args = args |> String.concat " "
            failwith $"Process '%s{command}' failed with nonzero exit code %i{proc.ExitCode}. Args: %s{args}"


// --------------------------------------------------------------------------------------
// Clean build results

type HaveCleaned = HaveCleaned
let doClean () : HaveCleaned =
    cleanDirectories ["bin" ; "temp" ; "output"]
    HaveCleaned

(*
Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"; "output"]
)
*)

// ===================

// Information about each project is used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

type ProjectInfo =
  { /// The name of the project 
    /// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
    Name : string
    /// Short summary of the project
    /// (used as description in AssemblyInfo and as a short summary for NuGet package)
    Summary : string
  }

//File that contains the release notes.
let releaseNotes = "FsCheck Release Notes.md"

/// Solution or project files to be built during the building process
let solution = "FsCheck.sln"

/// Pattern specifying assemblies to be tested
let testAssemblies = "tests/**/bin/Release/net6.0/*.Test.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fscheck"
let gitHome = sprintf "ssh://github.com/%s" gitOwner
// gitraw location - used for source linking
let gitRaw =
    match Environment.GetEnvironmentVariable "gitRaw" with
    | null | "" -> "https://raw.github.com/fscheck"
    | s -> s

// The name of the project on GitHub
let gitName = "FsCheck"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = Fake.Core.ReleaseNotes.load releaseNotes

let isAppVeyorBuild =
    String.IsNullOrEmpty (Environment.GetEnvironmentVariable "APPVEYOR_BUILD_VERSION")
    |> not
let buildDate = DateTime.UtcNow
let buildVersion : string =
    if not isAppVeyorBuild then
        release.NugetVersion
    else

    let repoVersion : string option =
        match Environment.GetEnvironmentVariable "APPVEYOR_REPO_TAG" with
        | null -> None
        | s ->
            if s.Equals ("true", StringComparison.OrdinalIgnoreCase) then
                match Environment.GetEnvironmentVariable "APPVEYOR_REPO_TAG_NAME" with
                | null -> failwith "AppVeyor unexpectedly told us we had a repo tag, but didn't give us its name"
                | s ->
                    match Version.TryParse s with
                    | false, _ -> None
                    | true, _ -> Some s
            else
                None

    match repoVersion with
    | None ->
        release.NugetVersion
    | Some repoVersion ->
        let buildNumber = Environment.GetEnvironmentVariable "APPVEYOR_BUILD_NUMBER"
        sprintf "%s-b%s" repoVersion buildNumber

let packages =
  [
    { Name = "FsCheck"
      Summary = "FsCheck is a tool for testing .NET programs automatically using randomly generated test cases."
    }
    { Name = "FsCheck.NUnit"
      Summary = "Integrates FsCheck with NUnit"
    }
    { Name = "FsCheck.Xunit"
      Summary = "Integrates FsCheck with xUnit.NET"
    }
  ]

type HaveUpdatedBuildVersion = | HaveUpdatedBuildVersion
let appveyorBuildVersion (_ : HaveCleaned) : HaveUpdatedBuildVersion =
    runProcess "appveyor" ["UpdateBuild" ; "-Version" ; buildVersion]
    HaveUpdatedBuildVersion

(*
Target.create "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)
*)

// Generate assembly info files with the right version & up-to-date information
type HaveGeneratedAssemblyInfo = | HaveGeneratedAssemblyInfo
let generateAssemblyInfo (_ : HaveCleaned) : HaveGeneratedAssemblyInfo =
    for package in packages do
        let fileName = $"src/%s{package.Name}/AssemblyInfo.fs"

        let shouldHaveInternalsVisibleTo =
            [
                "FsCheck"
                "FsCheck.Xunit"
            ]
            |> Set.ofList

        let ivt, ivtLiteral =
            if Set.contains package.Name shouldHaveInternalsVisibleTo then
                """[<Assembly: internalsVisibleTo("FsCheck.Test")>]""", "let [<Literal>] InternalsVisibleTo = \"FsCheck.Test\""
            else "", ""

        let assemblyInfoText = $"""
// Auto-generated; edits may be deleted at any time
namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitle("%s{package.Name}")>]
[<assembly: AssemblyProduct("%s{package.Name}")>]
[<assembly: AssemblyDescription(%s{package.Summary}")>]
[<assembly: AssemblyVersion("%s{release.AssemblyVersion}")>]
[<assembly: AssemblyFileVersion("%s{release.AssemblyVersion}")>]
[<assembly: AssemblyKeyFile("../../FsCheckKey.snk")>]
%s{ivt}
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] AssemblyTitle = "%s{package.Name}"
    let [<Literal>] AssemblyProduct = "%s{package.Name}"
    let [<Literal>] AssemblyDescription = "%s{package.Summary}"
    let [<Literal>] AssemblyVersion = "%s{release.AssemblyVersion}"
    let [<Literal>] AssemblyFileVersion = "%s{release.AssemblyVersion}"
    let [<Literal>] AssemblyKeyFile = "../../FsCheckKey.snk"
    %s{ivtLiteral}
"""
        File.WriteAllText (fileName, assemblyInfoText)
    HaveGeneratedAssemblyInfo

(*
Target.create "AssemblyInfo" (fun _ ->
    packages |> Seq.iter (fun package ->
    let fileName = "src/" + package.Name + "/AssemblyInfo.fs"
    
    AssemblyInfoFile.createFSharp fileName
        ([AssemblyInfo.Title package.Name
          AssemblyInfo.Product package.Name
          AssemblyInfo.Description package.Summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion
          AssemblyInfo.KeyFile "../../FsCheckKey.snk"
        ] @ (if package.Name = "FsCheck" || package.Name = "FsCheck.Xunit"
             then [AssemblyInfo.InternalsVisibleTo("FsCheck.Test")] else []))
    )
)
*)

// --------------------------------------------------------------------------------------
// Build library & test project

type HaveBuilt = HaveBuilt
let build (_ : HaveCleaned) : HaveBuilt =
    runProcess "dotnet" ["restore" ; solution]
    runProcess "dotnet" ["build" ; solution ; "--configuration" ; "Release"]
    HaveBuilt

(*
Target.create "Build" (fun _ ->
    DotNet.restore id solution
    DotNet.build (fun opt -> { opt with Configuration = DotNet.BuildConfiguration.Release }) solution
)
*)

// --------------------------------------------------------------------------------------
// Run the unit tests

type HaveTested = HaveTested
let runDotnetTest (_ : HaveCleaned) : HaveTested =
    runProcess "dotnet" ["test" ; "tests/FsCheck.Test" ; "--configuration" ; "Release"]
    HaveTested

(*
Target.create "RunTests" (fun _ ->
  "tests/FsCheck.Test/"
  |> DotNet.test (fun opt -> { opt with Configuration = DotNet.BuildConfiguration.Release })
)
*)

// --------------------------------------------------------------------------------------
// Build a NuGet package

type HavePacked = HavePacked
let packNuGet (_ : HaveTested) : HavePacked =
    let releaseNotes =
        release.Notes
        |> String.concat "\n"
    runProcess "dotnet" ["pack" ; "--configuration" ; "Release" ; $"-p:Version=%s{buildVersion}" ; "--output" ; "bin" ; $"-p:PackageReleaseNotes=%s{releaseNotes}"]
    HavePacked

(*
Target.create "PaketPack" (fun _ ->
    Paket.pack (fun p ->
      { p with
          OutputPath = "bin"
          Version = buildVersion
          ReleaseNotes = String.toLines release.Notes
          ToolType = ToolType.CreateLocalTool()
      })
)
*)

type HavePushed = HavePushed
let pushNuGet (_ : HaveTested) =
    failwith "TODO"
    HavePushed

(*
Target.create "PaketPush" (fun _ ->
    Paket.push (fun p ->
        { p with 
            WorkingDir = "bin"
            ToolType = ToolType.CreateLocalTool()
        })
)
*)

// --------------------------------------------------------------------------------------
// Generate the documentation

let fsdocParameters = [
  "fsdocs-release-notes-link https://github.com/fscheck/FsCheck/blob/master/FsCheck%20Release%20Notes.md"
  "fsdocs-license-link https://github.com/fscheck/FsCheck/blob/master/License.txt"
  "fsdocs-navbar-position fixed-left"
]

let fsdocProperties = [
  "Configuration=Release"
  "TargetFramework=netstandard2.0"
]

type HaveGeneratedDocs = HaveGeneratedDocs
let docs (_ : HaveBuilt) : HaveGeneratedDocs =
    cleanDirectories [".fsdocs"]

    [
        "fsdocs" ; "build" ; "--strict" ; "--eval" ; "--clean"
        "--projects" ; "src/FsCheck/FsCheck.fsproj"
        "--properties" ; yield! fsdocProperties
        "--parameters" ; yield! fsdocParameters
    ]
    |> runProcess "dotnet"

    HaveGeneratedDocs

(*
Target.create "Docs" (fun _ ->
    Shell.cleanDir ".fsdocs"
    DotNet.exec id "fsdocs" ("build --strict --eval --clean"
      + " --projects src/FsCheck/FsCheck.fsproj" 
      + " --properties " + String.Join(" ",fsdocProperties) 
      + " --parameters " + String.Join(" ", fsdocParameters)) |> checkResult
)
*)

let watchDocs () =
    cleanDirectories [".fsdocs"]

    [
        "fsdocs" ; "watch" ; "--eval"
        "--projects" ; "src/FsCheck/FsCheck.fsproj"
        "--properties" ; yield! fsdocProperties
        "--parameters" ; yield! fsdocParameters
    ]
    |> runProcess "dotnet"

(*
Target.create "WatchDocs" (fun _ ->
    Shell.cleanDir ".fsdocs"
    DotNet.exec id "fsdocs" ("watch --eval"
      + " --projects src/FsCheck/FsCheck.fsproj"
      + " --properties " + String.Join(" ",fsdocProperties)
      + " --parameters " + String.Join(" ", fsdocParameters)) |> checkResult
)
*)

// --------------------------------------------------------------------------------------
// Release Scripts

let rec copyDir (source : DirectoryInfo) (target : DirectoryInfo) : unit =
    target.Create ()
    for file in source.EnumerateFiles () do
        file.CopyTo (Path.Combine (target.FullName, file.Name)) |> ignore<FileInfo>
    for dir in source.EnumerateDirectories () do
        copyDir dir (Path.Combine (target.FullName, dir.Name) |> DirectoryInfo)

let releaseDocs (_ : HaveBuilt) =
    let tempDocsDir = "temp/gh-pages"
    cleanDirectories [tempDocsDir]
    let tempDocsDir = Directory.CreateDirectory tempDocsDir
    runProcess "git" ["clone" ; "git@github.com:fscheck/FsCheck.git" ; "--single-branch" ; "--branch" ; "gh-pages" ; tempDocsDir.FullName]

    copyDir (DirectoryInfo "output") tempDocsDir

    runProcess "git" ["--git-dir" ; tempDocsDir.FullName ; "commit" ; "--all" ; "--message" ; $"Update generated documentation for version %s{buildVersion}"]
    runProcess "git" ["--git-dir" ; tempDocsDir.FullName ; "push"]

(*
Target.create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    Shell.cleanDir tempDocsDir
    Repository.cloneSingleBranch "" ("git@github.com:fscheck/FsCheck.git") "gh-pages" tempDocsDir

    Repository.fullclean tempDocsDir
    Shell.copyRecursive "output" tempDocsDir true |> Trace.tracefn "%A"
    Staging.stageAll tempDocsDir
    Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" buildVersion)
    Branches.push tempDocsDir
)
*)

let release (_ : HaveTested) (_ : HaveGeneratedDocs) =
    failwith "TODO"

(*
Target.create "Release" (fun _ ->
    let user = Environment.environVarOrDefault "github-user" (UserInput.getUserInput "Username: ")
    let pw = Environment.environVarOrDefault "github-pw" (UserInput.getUserPassword "Password: ")
    let remote =
        CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Staging.stageAll ""
    Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    // release on github
    GitHub.createClient user pw
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // to upload a file: |> GitHub.uploadFiles "PATH_TO_FILE"
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)
*)

let runCi (_ : HaveGeneratedDocs) (_ : HaveTested) =
    // this is just a target to note that we have done all the above
    ()

let args = fsi.CommandLineArgs |> List.ofArray

match args with
| ["WatchDocs"] -> watchDocs ()
| [] | ["RunTests"] ->
    let haveCleaned = doClean ()
    runDotnetTest haveCleaned |> ignore<HaveTested>
| ["Clean"] -> doClean () |> ignore<HaveCleaned>
| ["CI"] ->
    let haveCleaned = doClean ()
    let haveBuilt = build haveCleaned
    let haveTested = runDotnetTest haveCleaned
    let haveGeneratedDocs = docs haveBuilt
    runCi haveGeneratedDocs haveTested
| ["Tests"] ->
    let haveCleaned = doClean ()
    runDotnetTest haveCleaned |> ignore<HaveTested>
| ["Docs"] ->
    let haveCleaned = doClean ()
    let haveBuilt = build haveCleaned
    releaseDocs haveBuilt
| ["ReleaseDocs"] ->
    if not isAppVeyorBuild then
        failwith "Refusing to release docs from CI"
    let haveCleaned = doClean ()
    let haveBuilt = build haveCleaned
    releaseDocs haveBuilt
| _ ->
    failwith "Unrecognised arguments."

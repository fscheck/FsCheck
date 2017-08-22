// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"./packages/FAKE/tools/FakeLib.dll"
#load "./packages/SourceLink.Fake/tools/SourceLink.fsx"

open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.Testing

open SourceLink

open System
open System.IO
open Fake.AppVeyor

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
let solution = if isMono then "FsCheck-mono.sln" else "FsCheck.sln"

/// Pattern specifying assemblies to be tested
let testAssemblies = "tests/**/bin/Release/*.Test.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fscheck"
let gitHome = sprintf "ssh://github.com/%s" gitOwner
// gitraw location - used for source linking
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/fscheck"
// The name of the project on GitHub
let gitName = "FsCheck"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = LoadReleaseNotes releaseNotes

let isAppVeyorBuild = buildServer = BuildServer.AppVeyor
let buildDate = DateTime.UtcNow
let buildVersion = 
    let isVersionTag tag = Version.TryParse tag |> fst
    let hasRepoVersionTag = isAppVeyorBuild && AppVeyorEnvironment.RepoTag && isVersionTag AppVeyorEnvironment.RepoTagName
    let assemblyVersion = if hasRepoVersionTag then AppVeyorEnvironment.RepoTagName else release.NugetVersion
    if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion AppVeyorEnvironment.BuildNumber
    else assemblyVersion

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

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    packages |> Seq.iter (fun package ->
    let fileName = "src/" + package.Name + "/AssemblyInfo.fs"
    CreateFSharpAssemblyInfo fileName
        ([Attribute.Title package.Name
          Attribute.Product package.Name
          Attribute.Description package.Summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
        ] @ (if package.Name = "FsCheck" || package.Name = "FsCheck.Xunit"
             then [Attribute.InternalsVisibleTo("FsCheck.Test")] else []))
    )
)

// --------------------------------------------------------------------------------------
// Clean build results


Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    !! solution
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    !! testAssemblies
    |> xUnit2 (fun p ->
            {p with
                ToolPath = "packages/xunit.runner.console/tools/xunit.console.exe"
                //The NoAppDomain setting requires care.
                //On mono, it needs to be true otherwise xunit won't work due to a Mono bug.
                //On .NET, it needs to be false otherwise Unquote won't work because it won't be able to load the FsCheck assembly.
                NoAppDomain = isMono
                ShadowCopy = false })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "PaketPack" (fun _ ->
    Paket.Pack (fun p ->
      { p with
          OutputPath = "bin"
          Version = buildVersion
          ReleaseNotes = toLines release.Notes
      })
)

Target "PaketPush" (fun _ ->
    Paket.Push (fun p ->
        { p with 
            WorkingDir = "bin"
        })
)

// --------------------------------------------------------------------------------------
// Generate the documentation

let generateHelp' fail debug =
    let args =
        if debug then ["--define:HELP"]
        else ["--define:RELEASE"; "--define:HELP"]
    if executeFSIWithArgs "docs/tools" "generate.fsx" args [] then
        traceImportant "Help generated"
    else
        if fail then
            failwith "generating help documentation failed"
        else
            traceImportant "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail true


Target "KeepRunning" (fun _ ->
    use watcher = new FileSystemWatcher(DirectoryInfo("docs/content").FullName,"*.fsx")
    watcher.EnableRaisingEvents <- true
    watcher.Changed.Add(fun e -> trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)
    watcher.Created.Add(fun e -> trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)
    watcher.Renamed.Add(fun e -> trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)
    //watcher.Deleted.Add(fun e -> trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)

    traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.EnableRaisingEvents <- false
    watcher.Dispose()
)

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"; "--define:HELP"; "--define:REFERENCE"] [] |> ignore
)
Target "GenerateDocsJa" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.ja.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" ("git@github.com:fscheck/FsCheck.git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" buildVersion)
    Branches.push tempDocsDir
)

#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit 

Target "Release" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // to upload a file: |> uploadFile "PATH_TO_FILE"
    |> releaseDraft
    |> Async.RunSynchronously
)



// --------------------------------------------------------------------------------------
// .NET Core SDK and .NET Core

let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x
let isDotnetSDKInstalled = try Shell.Exec("dotnet", "--version") = 0 with _ -> false
let shellExec cmd args dir =
    printfn "%s %s" cmd args
    Shell.Exec(cmd, args, dir) |> assertExitCodeZero

Target "Build.NetCore" (fun _ ->
    shellExec "dotnet" (sprintf "restore /p:Version=%s" buildVersion) "src/FsCheck.netcore"
    shellExec "dotnet" (sprintf "restore /p:Version=%s" buildVersion) "src/FsCheck.Xunit.netcore"
    shellExec "dotnet" (sprintf "restore /p:Version=%s" buildVersion) "src/FsCheck.NUnit.netcore"
    shellExec "dotnet" (sprintf "restore /p:Version=%s" buildVersion) "tests/FsCheck.Test.netcore"
    shellExec "dotnet" (sprintf "pack /p:Version=%s --configuration Release" buildVersion) "src/FsCheck.netcore"
    shellExec "dotnet" (sprintf "pack /p:Version=%s --configuration Release" buildVersion) "src/FsCheck.Xunit.netcore"
    shellExec "dotnet" (sprintf "pack /p:Version=%s --configuration Release" buildVersion) "src/FsCheck.NUnit.netcore"
)

Target "RunTests.NetCore" (fun _ ->
    shellExec "dotnet" "xunit" "tests/FsCheck.Test.netcore"
)

Target "Nuget.AddNetCore" (fun _ ->

    for name in [ "FsCheck"; "FsCheck.NUnit"; "FsCheck.Xunit" ] do
        let nupkg = sprintf "../../bin/%s.%s.nupkg" name buildVersion
        let netcoreNupkg = sprintf "bin/Release/%s.%s.nupkg" name buildVersion

        shellExec "dotnet" (sprintf """mergenupkg --source "%s" --other "%s" --framework netstandard1.6 """ nupkg netcoreNupkg) (sprintf "src/%s.netcore/" name)

)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "CI" DoNothing

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "Build"
  =?> ("Build.NetCore", isDotnetSDKInstalled)
  =?> ("RunTests.NetCore", isDotnetSDKInstalled)
  ==> "RunTests"

"RunTests"
  ==> "CleanDocs"
  ==> "GenerateDocsJa"
  ==> "GenerateDocs"
  =?> ("ReleaseDocs", isLocalBuild)
  ==> "Release"

"RunTests"
  ==> "PaketPack"
  =?> ("Nuget.AddNetCore", isDotnetSDKInstalled)
  ==> "PaketPush"
  ==> "Release"

"GenerateDocs"
  ==> "CI"

"RunTests"
  ==> "PaketPack" 
  =?> ("Nuget.AddNetCore", isDotnetSDKInstalled)
  ==> "CI"

RunTargetOrDefault "RunTests"

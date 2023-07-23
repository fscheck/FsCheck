// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------
#if FAKE
#r "paket: groupref Build //"
#endif
#load "./.fake/build.fsx/intellisense.fsx"

open System
open System.IO

open Fake.Api
open Fake.Core
open Fake.Core.TargetOperators
open Fake.BuildServer
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools.Git

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
let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.github.com/fscheck"
// The name of the project on GitHub
let gitName = "FsCheck"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = ReleaseNotes.load releaseNotes

let isAppVeyorBuild = BuildServer.buildServer = BuildServer.AppVeyor
let buildDate = DateTime.UtcNow
let buildVersion = 
    let isVersionTag (tag:string) = Version.TryParse tag |> fst
    let hasRepoVersionTag = isAppVeyorBuild && AppVeyor.Environment.RepoTag && isVersionTag AppVeyor.Environment.RepoTagName
    let assemblyVersion = if hasRepoVersionTag then AppVeyor.Environment.RepoTagName else release.NugetVersion
    if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion AppVeyor.Environment.BuildNumber
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

Target.create "BuildVersion" (fun _ -> 
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
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

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"; "output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    DotNet.restore id solution
    DotNet.build (fun opt -> { opt with Configuration = DotNet.BuildConfiguration.Release }) solution
)

// --------------------------------------------------------------------------------------
// Run the unit tests

let runAndCheck (f: unit -> ProcessResult) =
  let result = f()
  if not result.OK then
    failwithf "%A" result

Target.create "RunTests" (fun _ ->
  "tests/FsCheck.Test/"
  |> DotNet.test (fun opt -> { opt with Configuration = DotNet.BuildConfiguration.Release })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "PaketPack" (fun _ ->
    Paket.pack (fun p ->
      { p with
          OutputPath = "bin"
          Version = buildVersion
          ReleaseNotes = String.toLines release.Notes
          ToolType = ToolType.CreateLocalTool()
      })
)

Target.create "PaketPush" (fun _ ->
    Paket.push (fun p ->
        { p with 
            WorkingDir = "bin"
            ToolType = ToolType.CreateLocalTool()
        })
)

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

let checkResult (r:ProcessResult) =
  if not r.OK then
    failwithf "%A" r

Target.create "Docs" (fun _ ->
    Shell.cleanDir ".fsdocs"
    DotNet.exec id "fsdocs" ("build --strict --eval --clean"
      + " --projects src/FsCheck/FsCheck.fsproj" 
      + " --properties " + String.Join(" ",fsdocProperties) 
      + " --parameters " + String.Join(" ", fsdocParameters)) |> checkResult
)

Target.create "WatchDocs" (fun _ ->
    Shell.cleanDir ".fsdocs"
    DotNet.exec id "fsdocs" ("watch --eval"
      + " --projects src/FsCheck/FsCheck.fsproj" 
      + " --properties " + String.Join(" ",fsdocProperties) 
      + " --parameters " + String.Join(" ", fsdocParameters)) |> checkResult
)

// --------------------------------------------------------------------------------------
// Release Scripts

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

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "CI" ignore
Target.create "Tests" ignore

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"
  ==> "Tests"

"Build"
  ==> "Docs"

"Docs"
  =?> ("ReleaseDocs", BuildServer.isLocalBuild)
  ==> "Release"

"Tests"
  ==> "PaketPack"
  ==> "PaketPush"
  ==> "Release"

"Docs"
  ==> "CI"

"Tests"
  ==> "PaketPack"
  ==> "CI"

Target.runOrDefault "RunTests"

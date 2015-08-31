// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"./packages/FAKE/tools/NuGet.Core.dll"
#r @"./packages/FAKE/tools/FakeLib.dll"
#load "./packages/SourceLink.Fake/tools/SourceLink.fsx"

open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.Testing

open SourceLink

open System
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
    /// Longer description of the project
    /// (used as a description for NuGet package; line breaks are automatically cleaned up)
    Description : string
    /// List of author names (for NuGet package)
    Authors : string list
    /// Tags for your project (for NuGet package)
    Tags : string
    ///The projectfile (csproj or fsproj)
    ProjectFile : list<string>
    Dependencies : list<string * Lazy<string>>
  }

//File that contains the release notes.
let releaseNotes = "FsCheck Release Notes.md"

/// Solution or project files to be built during the building process
let solution = if isMono then "FsCheck-mono.sln" else "FsCheck.sln"

/// Pattern specifying assemblies to be tested
let testAssemblies = "tests/**/bin/Release/*.Test.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "ssh://github.com/fscheck"
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
    {
    Name = "FsCheck"
    Summary = "FsCheck is a tool for testing .NET programs automatically using randomly generated test cases."
    Description = """
FsCheck is a tool for testing .NET programs automatically. The programmer provides 
a specification of the program, in the form of properties which functions, methods 
or objects should satisfy, and FsCheck then tests that the properties hold in a 
large number of randomly generated cases. 
 
While writing the properties, you are actually writing a testable specification of your program. 
 
Specifications are expressed in F#, C# or VB, using combinators defined 
in the FsCheck library. FsCheck provides combinators to define properties, 
observe the distribution of test data, and define test data generators. 
When a property fails, FsCheck automatically displays a minimal counter example."""
    Authors = [ "Kurt Schelfthout and contributors" ]
    Tags = "test testing random fscheck quickcheck"
    ProjectFile = ["src/FsCheck/FsCheck.fsproj" ]
    Dependencies = ["FSharp.Core", lazy "3.1.2.5" ]
    
    }
    { 
    Name = "FsCheck.NUnit"
    Summary = "Integrates FsCheck with NUnit"
    Description = """FsCheck.NUnit integrates FsCheck with NUnit by adding a PropertyAttribute that runs FsCheck tests, similar to NUnit TestAttribute. 
    All the options normally available in vanilla FsCheck via configuration can be controlled via the PropertyAttribute."""
    Authors = [ "Kurt Schelfthout and contributors" ]
    Tags = "test testing random fscheck quickcheck nunit"
    ProjectFile = ["src/FsCheck.NUnit/FsCheck.NUnit.fsproj";"src/FsCheck.NUnit.Addin/FsCheck.NUnit.Addin.fsproj"]
    Dependencies = [ 
                    "NUnit",    lazy GetPackageVersion "./packages/" "NUnit"  //delayed so only runs after package restore step
                    "NUnit.Runners",    lazy GetPackageVersion "./packages/" "NUnit.Runners"
                    "FsCheck",  lazy buildVersion
                    ]     
    }
    { 
    Name = "FsCheck.Xunit"
    Summary = "Integrates FsCheck with xUnit.NET"
    Description = """
FsCheck.Xunit integrates FsCheck with xUnit.NET by adding a PropertyAttribute that runs FsCheck tests, similar to xUnit.NET's FactAttribute.
 
All the options normally available in vanilla FsCheck via configuration can be controlled via the PropertyAttribute."""
    Authors = [ "Kurt Schelfthout and contributors" ]
    Tags = "test testing random fscheck quickcheck xunit xunit.net"
    ProjectFile = ["src/FsCheck.Xunit/FsCheck.Xunit.fsproj"]
    Dependencies = [ 
                    "xunit", lazy GetPackageVersion "./packages/" "xunit"  //delayed so only runs after package restore step
                    "FsCheck",  lazy buildVersion
                     ]
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
        ] @ (if package.Name = "FsCheck" then [Attribute.InternalsVisibleTo("FsCheck.Test")] else []))
    )
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" RestorePackages

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
// temporary change to alt because support for the new xunit argument -noappdomain is required on mono
    !! testAssemblies
    |> xUnit2 (fun p ->
            {p with
                ToolPath = "packages/xunit.runner.console/tools/xunit.console.exe"
                NoAppDomain = true
                ShadowCopy = false })
)

// --------------------------------------------------------------------------------------
// Source linking

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw packages.[0].Name
    !! "src/**/*.??proj"
    |> Seq.iter (fun projFile ->
        let proj = VsProj.LoadRelease projFile 
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb __SOURCE_DIRECTORY__ baseUrl
    )
)
// --------------------------------------------------------------------------------------
// Build a NuGet package
type OptionalString = string option

open System
open System.IO

Target "NuGet" (fun _ ->        
    packages |> Seq.iter (fun package ->
    NuGet (fun p -> 
        { p with   
            Authors = package.Authors
            Project = package.Name
            Summary = package.Summary
            Description = package.Description
            Version = buildVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = package.Tags
            OutputPath = "bin"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies = package.Dependencies |> List.map (fun (name,dep) -> (name,dep.Force()))
        })
        ("nuget/" + package.Name + ".nuspec")
   )
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

Target "Release" (fun _ ->
    StageAll ""
    Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "CI" DoNothing

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"

"RunTests"  
  ==> "CleanDocs"
  ==> "GenerateDocsJa"  
  ==> "GenerateDocs"
  ==> "CI"
  =?> ("ReleaseDocs", isLocalBuild)
  ==> "Release"

"RunTests"
  =?> ("SourceLink", isLocalBuild && not isLinux)
  ==> "NuGet"
  ==> "Release"

RunTargetOrDefault "RunTests"

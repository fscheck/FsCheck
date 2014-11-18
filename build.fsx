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

open SourceLink

open System

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
    Files: list<string>
    //For extra files like tools and content string*string is for path target, if an excludes becomes necesary a third tuple element string option can be added
    FilesExtra: list<string * string >
  }

//File that contains the release notes.
let releaseNotes = "FsCheck Release Notes.md"

/// Solution or project files to be built during the building process
let solution = if isMono then "FsCheck-mono.sln" else "FsCheck.sln"

/// Pattern specifying assemblies to be tested
let testAssemblies = "tests/**/bin/Release/*.Test.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "ssh://github.com/fsharp"
// gitraw location - used for source linking
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/fsharp"
// The name of the project on GitHub
let gitName = "FsCheck"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines releaseNotes)

let packages =
  [
    {
    Name = "FsCheck"
    Files = ["FsCheck"]     
    FilesExtra = []
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
    Dependencies = []
    
    }
    { 
    Name = "FsCheck.Nunit"
    Files = ["FsCheck.Nunit"; "FsCheck.Nunit.Addin"]
    FilesExtra = [("FsCheck.Nunit.nuspec.tools\install.ps1", "tools\install.ps1");(@"FsCheck.Nunit.nuspec.tools\FsCheckAddin.fs","content\FsCheckAddin.fs")]
    Summary = "Integrates FsCheck with NUnit"
    Description = """FsCheck.NUnit integrates FsCheck with NUnit by adding a PropertyAttribute that runs FsCheck tests, similar to NUnit TestAttribute. 
    All the options normally available in vanilla FsCheck via configuration can be controlled via the PropertyAttribute."""
    Authors = [ "Kurt Schelfthout and contributors" ]
    Tags = "test testing random fscheck quickcheck nunit"
    ProjectFile = ["src/FsCheck.NUnit/FsCheck.NUnit.fsproj";"src/FsCheck.NUnit.Addin/FsCheck.NUnit.Addin.fsproj"]
    Dependencies = [ 
                    "NUnit",    lazy GetPackageVersion "./packages/" "NUnit"  //delayed so only runs after package restore step
                    "NUnit.Runners",    lazy GetPackageVersion "./packages/" "NUnit.Runners"
                    "FsCheck",  lazy release.AssemblyVersion
                    ]     
    }
    { 
    Name = "FsCheck.Xunit"
    Files = ["FsCheck.Xunit"]
    FilesExtra = []
    Summary = "Integrates FsCheck with xUnit.NET"
    Description = """
FsCheck.Xunit integrates FsCheck with xUnit.NET by adding a PropertyAttribute that runs FsCheck tests, similar to xUnit.NET's FactAttribute.
 
All the options normally available in vanilla FsCheck via configuration can be controlled via the PropertyAttribute."""
    Authors = [ "Kurt Schelfthout and contributors" ]
    Tags = "test testing random fscheck quickcheck xunit xunit.net"
    ProjectFile = ["src/FsCheck.Xunit/FsCheck.Xunit.fsproj"]
    Dependencies = [ 
                    "xunit", lazy GetPackageVersion "./packages/" "xunit"  //delayed so only runs after package restore step
                    "FsCheck",  lazy release.AssemblyVersion
                     ]
   }
  ]

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
    !! testAssemblies
    |> xUnit (fun p -> 
            {p with 
                ToolPath = "packages/xunit.runners.1.9.2/tools/xunit.console.clr4.exe"
                ShadowCopy = false
                HtmlOutput = not (isLinux || isMacOS)
                XmlOutput = false
                OutputDir = "temp" }) 
)

// --------------------------------------------------------------------------------------
// Source linking

Target "SourceLink" (fun _ ->
    use repo = new GitRepo(__SOURCE_DIRECTORY__)
    let LogAndVerify (proj: Microsoft.Build.Evaluation.Project) =
        logfn "source linking %s" proj.OutputFilePdb
        let files = proj.Compiles -- "**/AssemblyInfo.fs"
        repo.VerifyChecksums files
        proj.VerifyPdbChecksums files
        proj.CreateSrcSrv (sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName) repo.Revision (repo.Paths files)
        Pdbstr.exec proj.OutputFilePdb proj.OutputFilePdbSrcSrv
    packages 
    |> Seq.iter (fun f ->
        let proj = List.map VsProj.LoadRelease f.ProjectFile
        List.iter (fun p-> LogAndVerify p) proj
        
    )
)

// --------------------------------------------------------------------------------------
// Build a NuGet package
type OptionalString = string option

open System
Target "NuGet" (fun _ ->        
    let createFilesList (fileNames: string list) (extra: list<string*string> ) = 
        
        let createBinariesFiles fileNames =
            let extensions = [ "dll";"pdb";"XML"]            
            [for filename in fileNames do
                for ext in extensions do
                    yield (sprintf @"..\src\%s\bin\Release\%s.%s" filename filename ext, Some @"lib\net45", OptionalString.None)]
        let addExtras (extraFiles:list<string*string>) =
            extraFiles
            |>  List.map (fun f -> (fst f, Some (snd f), OptionalString.None))
                                   
                
        createBinariesFiles fileNames@addExtras extra 
   
    packages |> Seq.iter (fun package ->
    NuGet (fun p -> 
        { p with   
            Authors = package.Authors
            Project = package.Name
            Summary = package.Summary
            Description = package.Description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = package.Tags
            OutputPath = "bin"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            //ProjectFile = package.ProjectFile //if we add this, it produces a symbols package
            Dependencies = package.Dependencies |> List.map (fun (name,dep) -> (name,dep.Force()))
            Files = createFilesList package.Files package.FilesExtra           
        })
        ("nuget/" + package.Name + ".nuspec")
   )
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)
Target "GenerateDocsJa" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.ja.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" ("git@github.com:fsharp/FsCheck.git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
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

Target "All" DoNothing
Target "CI" DoNothing

"Clean"
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

"Build"
  ==> "CleanDocs"
  ==> "GenerateDocsJa"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"

"All"
  ==> "GenerateDocs"
  ==> "CI"

"All"
  ==> "ReleaseDocs"
  =?> ("SourceLink", isLocalBuild && not isLinux)
  ==> "NuGet"
  ==> "Release"

RunTargetOrDefault "All"

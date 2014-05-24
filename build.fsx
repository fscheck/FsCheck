// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
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
    ///File that contains the release notes.
    ReleaseNotes : string
    /// Solution or project files to be built during the building process
    FileToBuild : string
    /// Pattern specifying assemblies to be tested
    TestAssemblies : string
  }

let FsCheck =
  { Name = "FsCheck"
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
    ReleaseNotes = "FsCheck Release Notes.md"
    FileToBuild  = "src/FsCheck/FsCheck.fsproj"
    TestAssemblies = "tests/**/bin/Release/*.Test.dll"
 }

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/fsharp"
// The name of the project on GitHub
let gitName = "FsCheck"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines FsCheck.ReleaseNotes)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fileName = "src/" + FsCheck.Name + "/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
      [ Attribute.Title FsCheck.Name
        Attribute.Product FsCheck.Name
        Attribute.Description FsCheck.Summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ] 
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
    !! (FsCheck.FileToBuild)
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    !! FsCheck.TestAssemblies
    |> xUnit (fun p -> 
            {p with 
                ShadowCopy = false;
                HtmlOutput = true;
                XmlOutput = true;
                OutputDir = "temp" }) 
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    NuGet (fun p -> 
        { p with   
            Authors = FsCheck.Authors
            Project = FsCheck.Name
            Summary = FsCheck.Summary
            Description = FsCheck.Description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = FsCheck.Tags
            OutputPath = "bin"
            //AccessKey = getBuildParamOrDefault "nugetkey" ""
            //Publish = hasBuildParam "nugetkey"
            Dependencies = [] 
            Files = [(sprintf @"..\src\%s\bin\Release\%s.dll" FsCheck.Name FsCheck.Name, Some @"lib\net40-Client", None)
                     (sprintf @"..\src\%s\bin\Release\%s.XML" FsCheck.Name FsCheck.Name, Some @"lib\net40-Client", None)
                    ]
        })
        ("nuget/" + FsCheck.Name + ".nuspec")
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

Target "Release" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

"All" 
  //==> "CleanDocs"
  //==> "GenerateDocs"
  //==> "ReleaseDocs"
  ==> "NuGet"
  ==> "Release"

RunTargetOrDefault "All"

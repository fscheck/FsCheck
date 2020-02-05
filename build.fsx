// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r "paket:
nuget Octokit
nuget Fake.Api.GitHub
nuget Fake.BuildServer.AppVeyor
nuget Fake.Core.Environment
nuget Fake.Core.Process
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.SemVer
nuget Fake.Core.String
nuget Fake.Core.Target
nuget Fake.Core.Trace
nuget Fake.Core.UserInput
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Cli
nuget Fake.DotNet.FSFormatting
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Paket
nuget Fake.DotNet.Testing.XUnit2
nuget Fake.IO.FileSystem
nuget Fake.Tools.Git //"
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
let testAssemblies = "tests/**/bin/Release/net452/*.Test.dll"

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
        ] @ (if package.Name = "FsCheck" || package.Name = "FsCheck.Xunit"
             then [Fake.DotNet.AssemblyInfo.InternalsVisibleTo("FsCheck.Test")] else []))
    )
)

// --------------------------------------------------------------------------------------
// Clean build results


Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"]
)

Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    DotNet.restore id solution
    !! solution
    |> MSBuild.runRelease (fun par -> { par with MaxCpuCount = Some (Some Environment.ProcessorCount) }) "" "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target.create "RunTests" (fun _ ->
    !! testAssemblies
    |> XUnit2.run (fun p ->
            { p with
                //ToolPath = "packages/build/xunit.runner.console/tools/xunit.console.exe"
                //The NoAppDomain setting requires care.
                //On mono, it needs to be true otherwise xunit won't work due to a Mono bug.
                //On .NET, it needs to be false otherwise Unquote won't work because it won't be able to load the FsCheck assembly.
                NoAppDomain = Environment.isMono
                ShadowCopy = false })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "PaketPack" (fun _ ->
    Paket.pack (fun p ->
      { p with
          OutputPath = "bin"
          Version = buildVersion
          ReleaseNotes = String.toLines release.Notes
      })
)

Target.create "PaketPush" (fun _ ->
    Paket.push (fun p ->
        { p with 
            WorkingDir = "bin"
        })
)

// --------------------------------------------------------------------------------------
// Generate the documentation

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "src/FsCheck.Xunit/bin/Release/net452" //might not work in the future
let content    = __SOURCE_DIRECTORY__ @@ "docs/content"
let output     = __SOURCE_DIRECTORY__ @@ "docs/output"
let files      = __SOURCE_DIRECTORY__ @@ "docs/files"
let templates  = __SOURCE_DIRECTORY__ @@ "docs/tools/templates"
let formatting = __SOURCE_DIRECTORY__ @@ "packages/build/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

let github_release_user = Environment.environVarOrDefault "github_release_user" gitOwner
let githubLink = sprintf "https://github.com/%s/%s" github_release_user gitName

// Specify more information about your project
let info =
  [ "project-name", "FsCheck"
    "project-author", "Kurt Schelfthout and contributors"
    "project-summary", "FsCheck is a tool for testing .NET programs automatically using randomly generated test cases."
    "project-github", githubLink
    "project-nuget", "http://nuget.org/packages/FsCheck" ]

// Web site location for the generated documentation
let website = "/FsCheck"

let root = website

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceBinaries = [ "FsCheck.dll" ]

let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[   templates;
                            formatting @@ "templates"
                            formatting @@ "templates/reference" ])

Target.create "ReferenceDocs" (fun _ ->
    Directory.ensure (output @@ "reference")

    let binaries () =
        let manuallyAdded =
            referenceBinaries
            |> List.map (fun b -> bin @@ b)

        let conventionBased = []
            //DirectoryInfo.getSubDirectories <| DirectoryInfo bin
            //|> Array.collect (fun d ->
            //    let name, dInfo =
            //        let net45Bin =
            //            DirectoryInfo.getSubDirectories d |> Array.filter(fun x -> x.FullName.ToLower().Contains("net45"))
            //        let net47Bin =
            //            DirectoryInfo.getSubDirectories d |> Array.filter(fun x -> x.FullName.ToLower().Contains("net47"))
            //        if net45Bin.Length > 0 then
            //            d.Name, net45Bin.[0]
            //        else
            //            d.Name, net47Bin.[0]

            //    dInfo.GetFiles()
            //    |> Array.filter (fun x ->
            //        x.Name.ToLower() = (sprintf "%s.dll" name).ToLower())
            //    |> Array.map (fun x -> x.FullName)
            //    )
            //|> List.ofArray

        conventionBased @ manuallyAdded

    binaries()
    |> FSFormatting.createDocsForDlls (fun args ->
        { args with
            OutputDirectory = output @@ "reference"
            LayoutRoots =  layoutRootsAll.["en"]
            ProjectParameters =  ("root", root)::info
            SourceRepository = githubLink @@ "tree/master" }
           )
)

let copyFiles () =
    Shell.copyRecursive files output true
    |> Trace.logItems "Copying file: "
    Directory.ensure (output @@ "content")
    Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true
    |> Trace.logItems "Copying styles and scripts: "

/// Specifies the fsformatting executable
let toolPath =
    Fake.Core.ProcessUtils.tryFindLocalTool "FSFORMATTING" "fsformatting.exe" 
        [(Directory.GetCurrentDirectory() @@ "packages" @@ "build" @@ "FSharp.Formatting.CommandTool" @@ "tools")]
    |> Option.get

/// Runs fsformatting.exe with the given command in the given repository directory.
let private run toolPath command = 
    let result = CreateProcess.fromRawCommand toolPath [command]
                 |> CreateProcess.withFramework
                 |> Proc.run
    if result.ExitCode <> 0 then 
        failwithf "FSharp.Formatting %s failed." command

type LiterateArguments =
    { ToolPath : string
      Source : string
      OutputDirectory : string 
      Template : string
      ProjectParameters : (string * string) list
      LayoutRoots : string list }

let defaultLiterateArguments =
    { ToolPath = toolPath
      Source = ""
      OutputDirectory = ""
      Template = ""
      ProjectParameters = []
      LayoutRoots = [] }

let createDocs p =
    let arguments = (p:LiterateArguments->LiterateArguments) defaultLiterateArguments
    let layoutroots =
        if arguments.LayoutRoots.IsEmpty then []
        else [ "--layoutRoots" ] @ arguments.LayoutRoots
    let source = arguments.Source
    let template = arguments.Template
    let outputDir = arguments.OutputDirectory

    let command = 
        arguments.ProjectParameters
        |> Seq.map (fun (k, v) -> [ k; v ])
        |> Seq.concat
        |> Seq.append 
               (["literate"; "--processdirectory" ] @ layoutroots @ [ "--inputdirectory"; source; "--templatefile"; template; 
                  "--fsieval"; "--outputDirectory"; outputDir; "--replacements" ])
        |> Seq.map (fun s -> 
               if s.StartsWith "\"" then s
               else sprintf "\"%s\"" s)
        |> String.separated " "
    run arguments.ToolPath command
    printfn "Successfully generated docs for %s" source

Target.create "Docs" (fun _ ->
    //File.delete "docs/content/release-notes.md"
    //Shell.copyFile "docs/content/" "RELEASE_NOTES.md"
    //Shell.rename "docs/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    //File.delete "docsrc/content/license.md"
    //Shell.copyFile "docsrc/content/" "LICENSE.txt"
    //Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"


    DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
    |> Seq.iter (fun d ->
                    let name = d.Name
                    if name.Length = 2 || name.Length = 3 then
                        layoutRootsAll.Add(
                                name, [templates @@ name
                                       formatting @@ "templates"
                                       formatting @@ "templates/reference" ]))
    copyFiles ()

    for dir in  [ content; ] do
        let langSpecificPath(lang, path:string) =
            path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.exists(fun i -> i = lang)
        let layoutRoots =
            let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
            match key with
            | Some lang -> layoutRootsAll.[lang]
            | None -> layoutRootsAll.["en"] // "en" is the default language

        createDocs (fun args ->
            { args with 
                Source = content
                OutputDirectory = output
                LayoutRoots = layoutRoots
                ProjectParameters  = ("root", root)::info
                Template = docTemplate } )
)

//// --------------------------------------------------------------------------------------
//// Generate the documentation

//let generateHelp' fail debug =
//    let args =
//        if debug then ["--define:HELP"]
//        else ["--define:RELEASE"; "--define:HELP"]
//    if Fake.FSIHelper.executeFSIWithArgs "docs/tools" "generate.fsx" args [] then
//        Trace.traceImportant "Help generated"
//    else
//        if fail then
//            failwith "generating help documentation failed"
//        else
//            Trace.traceImportant "generating help documentation failed"

//let generateHelp fail =
//    generateHelp' fail true


//Target.create "KeepRunning" (fun _ ->
//    use watcher = new FileSystemWatcher(DirectoryInfo("docs/content").FullName,"*.fsx")
//    watcher.EnableRaisingEvents <- true
//    watcher.Changed.Add(fun e -> Trace.trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)
//    watcher.Created.Add(fun e -> Trace.trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)
//    watcher.Renamed.Add(fun e -> Trace.trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)
//    //watcher.Deleted.Add(fun e -> trace (sprintf "%A %A" e.Name e.ChangeType); generateHelp false)

//    Trace.traceImportant "Waiting for help edits. Press any key to stop."

//    System.Console.ReadKey() |> ignore

//    watcher.EnableRaisingEvents <- false
//    watcher.Dispose()
//)

//Target.create "GenerateDocs" (fun _ ->
//    Fake.FSIHelper.executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"; "--define:HELP"; "--define:REFERENCE"] [] |> ignore
//)
//Target.create "GenerateDocsJa" (fun _ ->
//    Fake.FSIHelper.executeFSIWithArgs "docs/tools" "generate.ja.fsx" ["--define:RELEASE"] [] |> ignore
//)

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    Shell.cleanDir tempDocsDir
    Repository.cloneSingleBranch "" ("git@github.com:fscheck/FsCheck.git") "gh-pages" tempDocsDir

    Repository.fullclean tempDocsDir
    Shell.copyRecursive "docs/output" tempDocsDir true |> Trace.tracefn "%A"
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
  ==> "ReferenceDocs"

"CleanDocs"
  ==> "ReferenceDocs"
  ==> "Docs"
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

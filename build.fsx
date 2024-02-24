// TODO: delete this reference, it makes the script take ages to start
#r "nuget:Fake.Core.ReleaseNotes"

open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

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

    let runProcessWithOutput (command : string) (args : string seq) : string =
        let psi = ProcessStartInfo (command, args)
        psi.RedirectStandardError <- true
        psi.RedirectStandardOutput <- true
        use proc = new Process ()
        proc.StartInfo <- psi
        if not (proc.Start ()) then
            let args = args |> String.concat " "
            failwith $"Failed to start process '%s{command}' with args: %s{args}"
        proc.WaitForExit ()
        let stdout =
            let stdout = proc.StandardOutput.ReadToEnd ()
            if String.IsNullOrWhiteSpace stdout then "" else $"\nStdout:\n  %s{stdout}"

        if proc.ExitCode <> 0 then
            let args = args |> String.concat " "
            let stderr =
                let stderr = proc.StandardOutput.ReadToEnd ()
                if String.IsNullOrWhiteSpace stderr then "" else $"\nStderr:\n  %s{stderr}"
            failwith $"Process '%s{command}' failed with nonzero exit code %i{proc.ExitCode}. Args: %s{args}.%s{stdout}%s{stderr}"

        stdout

    let runProcess command args = runProcessWithOutput command args |> ignore<string>

    let rec copyDir (source : DirectoryInfo) (target : DirectoryInfo) : unit =
        target.Create ()
        for file in source.EnumerateFiles () do
            file.CopyTo (Path.Combine (target.FullName, file.Name)) |> ignore<FileInfo>
        for dir in source.EnumerateDirectories () do
            copyDir dir (Path.Combine (target.FullName, dir.Name) |> DirectoryInfo)

// --------------------------------------------------------------------------------------
// Clean build results

type HaveCleaned = HaveCleaned
let doClean () : HaveCleaned =
    printf "Cleaning... "
    cleanDirectories ["bin" ; "temp" ; "output"]
    printfn "done."
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
let releaseNotesFile = "FsCheck Release Notes.md"

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
let releaseNotes = Fake.Core.ReleaseNotes.load releaseNotesFile

let isAppVeyorBuild =
    String.IsNullOrEmpty (Environment.GetEnvironmentVariable "APPVEYOR_BUILD_VERSION")
    |> not
let buildDate = DateTime.UtcNow
let buildVersion : string =
    if not isAppVeyorBuild then
        releaseNotes.NugetVersion
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
        releaseNotes.NugetVersion
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
    printf "appveyor UpdateBuild... "
    runProcess "appveyor" ["UpdateBuild" ; "-Version" ; buildVersion]
    printfn "done."
    HaveUpdatedBuildVersion

(*
Target.create "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)
*)

// Generate assembly info files with the right version & up-to-date information
type HaveGeneratedAssemblyInfo = | HaveGeneratedAssemblyInfo
let generateAssemblyInfo (_ : HaveCleaned) : HaveGeneratedAssemblyInfo =
    printf "Generating AssemblyInfo files... "
    for package in packages do
        let fileName = $"src/%s{package.Name}/AssemblyInfo.fs"
        printf $"(%s{fileName}) "

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
[<assembly: AssemblyVersion("%s{releaseNotes.AssemblyVersion}")>]
[<assembly: AssemblyFileVersion("%s{releaseNotes.AssemblyVersion}")>]
[<assembly: AssemblyKeyFile("../../FsCheckKey.snk")>]
%s{ivt}
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] AssemblyTitle = "%s{package.Name}"
    let [<Literal>] AssemblyProduct = "%s{package.Name}"
    let [<Literal>] AssemblyDescription = "%s{package.Summary}"
    let [<Literal>] AssemblyVersion = "%s{releaseNotes.AssemblyVersion}"
    let [<Literal>] AssemblyFileVersion = "%s{releaseNotes.AssemblyVersion}"
    let [<Literal>] AssemblyKeyFile = "../../FsCheckKey.snk"
    %s{ivtLiteral}
"""
        File.WriteAllText (fileName, assemblyInfoText)
    printfn "done."
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
    printf "Performing dotnet restore... "
    runProcess "dotnet" ["restore" ; solution]
    printfn "done."
    printf "Performing dotnet build... "
    runProcess "dotnet" ["build" ; solution ; "--configuration" ; "Release"]
    printfn "done."
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
    printf "Performing dotnet test... "
    runProcess "dotnet" ["test" ; "tests/FsCheck.Test" ; "--configuration" ; "Release"]
    printfn "done."
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
    printf "Performing dotnet pack... "
    let releaseNotes =
        releaseNotes.Notes
        |> String.concat "\n"
    runProcess "dotnet" ["pack" ; "--configuration" ; "Release" ; $"-p:Version=%s{buildVersion}" ; "--output" ; "bin" ; $"-p:PackageReleaseNotes=%s{releaseNotes}"]
    printfn "done."
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
    printf "Performing NuGet push... "
    failwith "TODO"
    printfn "done."
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
    printf "Running fsdocs build... "
    cleanDirectories [".fsdocs"]

    [
        "fsdocs" ; "build" ; "--strict" ; "--eval" ; "--clean"
        "--projects" ; "src/FsCheck/FsCheck.fsproj"
        "--properties" ; yield! fsdocProperties
        "--parameters" ; yield! fsdocParameters
    ]
    |> runProcess "dotnet"

    printfn "done."

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
    printf "Running fsdocs watch... "
    cleanDirectories [".fsdocs"]

    [
        "fsdocs" ; "watch" ; "--eval"
        "--projects" ; "src/FsCheck/FsCheck.fsproj"
        "--properties" ; yield! fsdocProperties
        "--parameters" ; yield! fsdocParameters
    ]
    |> runProcess "dotnet"
    printfn "fsdocs watch completed."

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

let releaseDocs (_ : HaveBuilt) =
    printf "Releasing docs to gh-pages branch... "
    let tempDocsDir = "temp/gh-pages"
    cleanDirectories [tempDocsDir]
    let tempDocsDir = Directory.CreateDirectory tempDocsDir
    runProcess "git" ["clone" ; "git@github.com:fscheck/FsCheck.git" ; "--single-branch" ; "--branch" ; "gh-pages" ; tempDocsDir.FullName]

    copyDir tempDocsDir (DirectoryInfo "output")

    runProcess "git" ["--git-dir" ; Path.Combine (tempDocsDir.FullName, ".git") ; "commit" ; "--all" ; "--message" ; $"Update generated documentation for version %s{buildVersion}"]
    runProcess "git" ["--git-dir" ; Path.Combine (tempDocsDir.FullName, ".git") ; "push"]
    printfn "done."

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

let rec getUserInput (prompt : string) =
    printfn $"%s{prompt}"
    let line = Console.ReadLine ()
    if String.IsNullOrWhiteSpace line then
        printfn "Entry must be non-whitespace."
        getUserInput prompt
    else
        line

type GitHubAsset =
    {
        BrowserDownloadUrl : Uri
        Name : string
        ContentType : string
        Size : uint64
    }

type GitHubReleaseResponse =
    {
        Name : string
        PublishedAt : DateTime
        Assets : GitHubAsset list
    }

type GitHubReleaseRequest =
    {
        [<JsonPropertyName "tag_name">]
        TagName : string
        [<JsonPropertyName "target_commitish">]
        TargetCommitish : string
        [<JsonPropertyName "name">]
        Name : string
        [<JsonPropertyName "body">]
        Body : string
        [<JsonPropertyName "draft">]
        IsDraft : bool
        [<JsonPropertyName "prerelease">]
        IsPreRelease : bool
    }

let release (_ : HaveTested) (_ : HaveGeneratedDocs) =
    printfn "Tagging new version and performing GitHub release... "

    let pat =
        match Environment.GetEnvironmentVariable "github-pat" with
        | null ->
            [
                "Obtain a GitHub personal access token from https://github.com/settings/tokens?type=beta ."
                $"Scope it to at least %s{gitOwner}/%s{gitName} and write access to Repository Permissions -> Contents."
            ]
            |> List.iter Console.WriteLine
            getUserInput "GitHub PAT: "
        | s -> s
    let remote =
        let matchingRemotes =
            runProcessWithOutput "git" ["remote" ; "-v"]
            |> fun stdout -> stdout.Split '\n'
            |> Seq.choose (fun line ->
                let line = line.Trim ()
                if line.EndsWith ("(push)", StringComparison.Ordinal) then
                    if line.Contains $"%s{gitOwner}/%s{gitName}" then
                        Some line
                    else None
                else None
            )
            |> Seq.toList
        match matchingRemotes with
        | [] -> $"%s{gitHome}/%s{gitName}"
        | [x] -> x.Split().[0]
        | _ ->
            let remotes =
                matchingRemotes
                |> String.concat " "
            failwith $"Multiple matching push remotes found for %s{gitOwner}/%s{gitName}: %s{remotes}"

    runProcess "git" ["commit" ; "--all" ; "--message" ; $"Bump version to %s{releaseNotes.NugetVersion}"]
    let gitBranch = runProcessWithOutput "git" ["symbolic-ref" ; "--short" ; "HEAD"]
    runProcess "git" ["push" ; remote ; gitBranch]
    runProcess "git" ["tag" ; releaseNotes.NugetVersion]
    runProcess "git" ["push" ; remote ; "--tag" ; releaseNotes.NugetVersion]

    printfn "Creating GitHub release... "

    let releaseSpec =
        {
            TagName = releaseNotes.NugetVersion
            TargetCommitish = "" // the default branch
            Name = releaseNotes.NugetVersion
            Body = releaseNotes.Notes |> String.concat "\n"
            IsDraft = true
            IsPreRelease = releaseNotes.SemVer.PreRelease.IsSome
        }

    use client = new HttpClient ()
    client.DefaultRequestHeaders.Add ("Accept", "application/vnd.github+json")
    client.DefaultRequestHeaders.Add ("X-GitHub-Api-Version", "2022-11-28")
    client.DefaultRequestHeaders.Authorization <- AuthenticationHeaderValue ("Bearer", pat)
    client.DefaultRequestHeaders.Add ("Authorization", "2022-11-28")
    let postData = JsonSerializer.Serialize releaseSpec
    use content = new StringContent (postData, Encoding.UTF8, "application/json")
    use response = client.PostAsync($"https://api.github.com/repos/%s{gitOwner}/%s{gitName}/releases", content).Result.EnsureSuccessStatusCode()
    let response = response.Content.ReadAsStringAsync().Result
    let output = JsonSerializer.Deserialize<GitHubReleaseResponse> response

    printfn $"GitHub response: %+A{output}"

    printfn "done."

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

let args =
    match fsi.CommandLineArgs |> List.ofArray with
    | "build.fsx" :: rest -> rest
    | args -> args

match args with
| ["-t" ; "WatchDocs"] -> watchDocs ()
| [] | ["-t" ; "RunTests"] ->
    let haveCleaned = doClean ()
    runDotnetTest haveCleaned |> ignore<HaveTested>
| ["-t"; "Clean"] -> doClean () |> ignore<HaveCleaned>
| ["-t"; "CI"] ->
    let haveCleaned = doClean ()
    let haveBuilt = build haveCleaned
    let haveTested = runDotnetTest haveCleaned
    let haveGeneratedDocs = docs haveBuilt
    runCi haveGeneratedDocs haveTested
| ["-t"; "Tests"] ->
    let haveCleaned = doClean ()
    runDotnetTest haveCleaned |> ignore<HaveTested>
| ["-t"; "Docs"] ->
    let haveCleaned = doClean ()
    let haveBuilt = build haveCleaned
    releaseDocs haveBuilt
| ["-t"; "ReleaseDocs"] ->
    if not isAppVeyorBuild then
        failwith "Refusing to release docs from CI"
    let haveCleaned = doClean ()
    let haveBuilt = build haveCleaned
    releaseDocs haveBuilt
| _ ->
    let args =
        args
        |> String.concat " "
    failwith $"Unrecognised arguments: %s{args}"

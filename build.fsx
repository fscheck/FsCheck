// TODO: delete this reference, it makes the script take ages to start
#r "nuget:Fake.Core.ReleaseNotes"

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Net.Http.Headers
open System.Runtime.InteropServices
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks

[<AutoOpen>]
module Utils =
    let cleanDirectories (relativeDirNames : string list) : unit =
        relativeDirNames
        |> List.iter (fun dirName ->
            let di = DirectoryInfo dirName
            if di.Exists then
                di.GetFileSystemInfos("*", SearchOption.AllDirectories) |> Array.iter (fun info -> info.Attributes <- FileAttributes.Normal)
                try
                    di.Delete true
                with :? DirectoryNotFoundException -> ()
        )

    let rec containsTaskCancellation (exc : Exception) =
        match exc with
        | :? TaskCanceledException -> true
        | :? AggregateException as e ->
            e.InnerExceptions
            |> Seq.exists containsTaskCancellation
        | _ -> false

    type Waiter (stdout : ResizeArray<string>, stderr : ResizeArray<string>) =
        let rec go () : Async<unit> = async {
            do! Async.Sleep (TimeSpan.FromSeconds 10.0)
            let lastStdout =
                lock stdout (fun () -> if stdout.Count = 0 then None else Some stdout.[stdout.Count - 1])
                |> Option.defaultValue "<no output yet>"
            Console.WriteLine $"Last stdout: %s{lastStdout}"
            let lastStderr =
                lock stderr (fun () -> if stderr.Count = 0 then None else Some stderr.[stderr.Count - 1])
                |> Option.defaultValue "<no error output yet>"
            Console.WriteLine $"Last stderr: %s{lastStderr}"
            return! go ()
        }
        let cts = new CancellationTokenSource ()

        let task = Async.StartAsTask (go (), cancellationToken = cts.Token)

        interface IDisposable with
            member _.Dispose () =
                cts.Cancel ()
                try
                    task.Wait ()
                with
                | e when containsTaskCancellation e -> ()

    let runProcessWithOutput (env : Map<string, string>) (command : string) (args : string seq) : string =
        let psi = ProcessStartInfo (command, args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        env
        |> Map.iter (fun key value ->
            psi.EnvironmentVariables.[key] <- value
        )
        use proc = new Process ()
        proc.StartInfo <- psi
        // have to do the dance to avoid the classic deadlock condition where the subprocess is waiting to push
        // to a full output buffer, but we aren't doing anything to clear the output buffer until the subprocess
        // quits
        let stdoutArr = ResizeArray ()
        proc.OutputDataReceived.Add (fun evt -> lock stdoutArr (fun () -> stdoutArr.Add evt.Data))
        let stderrArr = ResizeArray ()
        proc.ErrorDataReceived.Add (fun evt -> lock stderrArr (fun () -> stderrArr.Add evt.Data))

        if not (proc.Start ()) then
            let args = args |> String.concat " "
            failwith $"Failed to start process '%s{command}' with args: %s{args}"
        proc.BeginOutputReadLine ()
        proc.BeginErrorReadLine ()
        Console.Write $"(started process %i{proc.Id}) "

        do
            use waiter = new Waiter (stdoutArr, stderrArr)
            proc.WaitForExit ()

        let stdout = stdoutArr |> Seq.filter (fun s -> not (String.IsNullOrEmpty s)) |> String.concat " "

        if proc.ExitCode <> 0 then
            let args = args |> String.concat " "
            let stdout = if String.IsNullOrEmpty stdout then "" else $"\nStdout:\n  %s{stdout}"
            failwith $"Process '%s{command}' failed with nonzero exit code %i{proc.ExitCode}. Args: %s{args}.%s{stdout}"

        stdout

    let inline runProcessWithEnv env command args : unit = runProcessWithOutput env command args |> ignore<string>
    let inline runProcess command args : unit = runProcessWithEnv Map.empty command args

    let rec copyDir (source : DirectoryInfo) (target : DirectoryInfo) (overwrite : bool): unit =
        target.Create ()
        for file in source.EnumerateFiles () do
            file.CopyTo(Path.Combine (target.FullName, file.Name), overwrite) |> ignore<FileInfo>
        for dir in source.EnumerateDirectories () do
            copyDir dir (Path.Combine (target.FullName, dir.Name) |> DirectoryInfo) overwrite

// --------------------------------------------------------------------------------------
// Clean build results

type HaveCleaned = HaveCleaned
let doClean () : HaveCleaned =
    Console.Write "Cleaning... "
    cleanDirectories ["bin" ; "temp" ; "output"]
    Console.WriteLine "done."
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
    Console.Write "appveyor UpdateBuild... "
    runProcess "appveyor" ["UpdateBuild" ; "-Version" ; buildVersion]
    Console.WriteLine "done."
    HaveUpdatedBuildVersion

(*
Target.create "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)
*)

// Generate assembly info files with the right version & up-to-date information
type HaveGeneratedAssemblyInfo = | HaveGeneratedAssemblyInfo
let generateAssemblyInfo (_ : HaveCleaned) : HaveGeneratedAssemblyInfo =
    Console.Write "Generating AssemblyInfo files... "
    for package in packages do
        let fileName = $"src/%s{package.Name}/AssemblyInfo.fs"
        Console.Write $"(%s{fileName}) "

        let shouldHaveInternalsVisibleTo =
            [
                "FsCheck"
                "FsCheck.Xunit"
            ]
            |> Set.ofList

        let ivt, ivtLiteral =
            if Set.contains package.Name shouldHaveInternalsVisibleTo then
                """[<assembly: InternalsVisibleTo("FsCheck.Test")>]""", "let [<Literal>] InternalsVisibleTo = \"FsCheck.Test\""
            else "", ""

        let assemblyInfoText = $"""
// Auto-generated; edits may be deleted at any time
namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitle("%s{package.Name}")>]
[<assembly: AssemblyProduct("%s{package.Name}")>]
[<assembly: AssemblyDescription("%s{package.Summary}")>]
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
    Console.WriteLine "done."
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
let build (_ : HaveCleaned) (_ : HaveGeneratedAssemblyInfo) : HaveBuilt =
    Console.Write "Performing dotnet restore... "
    runProcess "dotnet" ["restore" ; solution]
    Console.WriteLine "done."
    Console.Write "Performing dotnet build... "
    runProcess "dotnet" ["build" ; solution ; "--configuration" ; "Release"]
    Console.WriteLine "done."
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
    Console.Write "Performing dotnet test... "
    runProcess "dotnet" ["test" ; "tests/FsCheck.Test" ; "--configuration" ; "Release"]
    Console.WriteLine "done."
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
    Console.Write "Performing dotnet pack... "
    let releaseNotes =
        releaseNotes.Notes
        |> String.concat "\n"
    runProcess "dotnet" ["pack" ; "--configuration" ; "Release" ; $"-p:Version=%s{buildVersion}" ; "--output" ; "bin" ; $"-p:PackageReleaseNotes=%s{releaseNotes}"]

    // I *believe* it is impossible to have a fixed (not floating) version number from a source reference
    // via `dotnet pack`. Without this next bit, FsCheck.Xunit vA.B.C depends on >= FsCheck vA.B.C, not
    // on FsCheck exactly at vA.B.C.

    for package in ["FsCheck.Xunit" ; "FsCheck.NUnit"] do
        let nupkg = FileInfo (Path.Combine ("bin", $"%s{package}.%s{buildVersion}.nupkg"))
        let stream = nupkg.Open (FileMode.Open, FileAccess.ReadWrite)
        use archive = new ZipArchive (stream, ZipArchiveMode.Update)
        let existingEntry = archive.GetEntry $"%s{package}.nuspec"
        let desiredContents =
            use nuspecContents = existingEntry.Open ()
            let reader = new StreamReader (nuspecContents)
            let contents = reader.ReadToEnd ()
            contents.Replace ($" version=\"%s{buildVersion}\"", $" version=\"[%s{buildVersion}]\"")
        existingEntry.Delete ()
        let newEntry = archive.CreateEntry $"%s{package}.nuspec"
        do
            let newContents = newEntry.Open ()
            use writer = new StreamWriter (newContents)
            writer.Write desiredContents

    Console.WriteLine "done."
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
let pushNuGet (_ : HaveTested) (_ : HavePacked) =
    Console.Write "Performing NuGet push... "
    let nugetKey =
        match Environment.GetEnvironmentVariable "NUGET_KEY" with
        | null ->
                failwith "No NuGet key in environment for NuGet publish. Set NUGET_KEY."
        | s -> s

    let env =
        ["NUGET_KEY", nugetKey]
        |> Map.ofList

    for package in ["FsCheck" ; "FsCheck.NUnit" ; "FsCheck.Xunit"] do
        let package = Path.Combine ("bin", $"%s{package}.%s{buildVersion}.nupkg")
        // yup, `dotnet nuget` really does have no way to pass in the critical secret secretly, so we have
        // to get the shell to do it
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            runProcessWithEnv env "cmd" ["/c" ; $"dotnet nuget push %s{package} --api-key %%NUGET_KEY%% --source https://api.nuget.org/v3/index.json"]
        else
            runProcessWithEnv env "sh" ["-c" ; $"dotnet nuget push %s{package} --api-key $NUGET_KEY --source https://api.nuget.org/v3/index.json"]
    Console.WriteLine "done."
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

let fsdocParameters =
    [
        "fsdocs-release-notes-link"; "https://github.com/fscheck/FsCheck/blob/master/FsCheck%20Release%20Notes.md"
        "fsdocs-license-link"; "https://github.com/fscheck/FsCheck/blob/master/License.txt"
        "fsdocs-navbar-position"; "fixed-left"
    ]

let fsdocProperties = [
  "Configuration=Release"
  "TargetFramework=netstandard2.0"
]

type HaveGeneratedDocs = HaveGeneratedDocs
let docs (_ : HaveBuilt) : HaveGeneratedDocs =
    Console.Write "Running fsdocs build... "
    cleanDirectories [".fsdocs"]

    [
        "fsdocs" ; "build" ; "--strict" ; "--eval" ; "--clean"
        "--projects" ; "src/FsCheck/FsCheck.fsproj"
        "--properties" ; yield! fsdocProperties
        "--parameters" ; yield! fsdocParameters
    ]
    |> runProcess "dotnet"

    let cwd = Environment.CurrentDirectory
    Console.WriteLine $"done (access at %s{cwd}/output/index.html)."

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
    Console.Write "Running fsdocs watch... "
    cleanDirectories [".fsdocs"]

    [
        "fsdocs" ; "watch" ; "--eval"
        "--projects" ; "src/FsCheck/FsCheck.fsproj"
        "--properties" ; yield! fsdocProperties
        "--parameters" ; yield! fsdocParameters
    ]
    |> runProcess "dotnet"
    Console.WriteLine "fsdocs watch completed."

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

let releaseDocs (_ : HaveGeneratedDocs) =
    Console.Write "Releasing docs to gh-pages branch... "
    let tempDocsDir = "temp/gh-pages"
    cleanDirectories [tempDocsDir]
    let tempDocsDir = Directory.CreateDirectory tempDocsDir
    runProcess "git" ["clone" ; "git@github.com:fscheck/FsCheck.git" ; "--single-branch" ; "--branch" ; "gh-pages" ; tempDocsDir.FullName]

    copyDir (DirectoryInfo "output") tempDocsDir true

    runProcess "git" ["-C" ; tempDocsDir.FullName ; "add" ; "--all"]
    runProcess "git" ["-C" ; tempDocsDir.FullName ; "commit" ; "--all" ; "--message" ; $"Update generated documentation for version %s{buildVersion}"]
    runProcess "git" ["-C" ; tempDocsDir.FullName ; "push"]
    Console.WriteLine "done."

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
    Console.Write $"%s{prompt}: "
    let line = Console.ReadLine ()
    if String.IsNullOrWhiteSpace line then
        Console.WriteLine "Entry must be non-whitespace."
        getUserInput prompt
    else
        line

type GitHubAsset =
    {
        [<JsonPropertyName "browser_download_url">]
        BrowserDownloadUrl : Uri
        [<JsonPropertyName "name">]
        Name : string
        [<JsonPropertyName "content_type">]
        ContentType : string
        [<JsonPropertyName "size">]
        Size : uint64
    }

type GitHubReleaseResponse =
    {
        [<JsonPropertyName "name">]
        Name : string
        [<JsonPropertyName "published_at">]
        PublishedAt : System.Nullable<DateTime>
        [<JsonPropertyName "assets">]
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

let gitHubRelease (_ : HaveTested) =
    Console.WriteLine "Tagging new version and performing GitHub release... "

    let pat =
        match Environment.GetEnvironmentVariable "GITHUB_PAT" with
        | null ->
            [
                "Obtain a GitHub personal access token from https://github.com/settings/tokens?type=beta ."
                $"Scope it to at least %s{gitOwner}/%s{gitName} and write access to Repository Permissions -> Contents."
            ]
            |> List.iter Console.WriteLine
            getUserInput "GitHub PAT"
        | s -> s
    let remote =
        let matchingRemotes =
            runProcessWithOutput Map.empty "git" ["remote" ; "-v"]
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

    runProcess "git" ["commit" ; "--all" ; "--allow-empty" ; "--message" ; $"Bump version to %s{releaseNotes.NugetVersion}"]
    let gitBranch =
        runProcessWithOutput Map.empty "git" ["symbolic-ref" ; "--short" ; "HEAD"]
    Console.WriteLine $"Branch: '%s{gitBranch}'; remote: '%s{remote}'"
    runProcess "git" ["push" ; remote ; gitBranch]
    runProcess "git" ["tag" ; "-f" ; releaseNotes.NugetVersion]
    runProcess "git" ["push" ; "-f" ; remote ; releaseNotes.NugetVersion]

    Console.WriteLine "Creating GitHub release... "

    let releaseSpec =
        {
            TagName = releaseNotes.NugetVersion
            TargetCommitish = "" // if empty, uses the default branch
            Name = releaseNotes.NugetVersion
            Body = releaseNotes.Notes |> String.concat "\n"
            IsDraft = true
            IsPreRelease = releaseNotes.SemVer.PreRelease.IsSome
        }

    use client = new HttpClient ()
    use message = new HttpRequestMessage (Method = HttpMethod.Post, RequestUri = Uri $"https://api.github.com/repos/%s{gitOwner}/%s{gitName}/releases")
    message.Headers.Add ("Accept", "application/vnd.github+json")
    message.Headers.Add ("X-GitHub-Api-Version", "2022-11-28")
    message.Headers.Add ("Authorization", $"Bearer %s{pat}")
    message.Headers.Add ("User-Agent", "build-fsx-release")
    let postData = JsonSerializer.Serialize releaseSpec
    use content = new StringContent (postData, Encoding.UTF8, "application/json")
    message.Content <- content
    use response = client.SendAsync(message).Result
    let response = response.Content.ReadAsStringAsync().Result
    Console.WriteLine $"GitHub raw response: %s{response}"
    let output = JsonSerializer.Deserialize<GitHubReleaseResponse> response

    Console.WriteLine $"Parsed GitHub response: %+A{output}"

    Console.WriteLine "done."

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

match args |> List.map (fun s -> s.ToLowerInvariant ()) with
| [] | ["-t" ; "runtests"] ->
    let haveCleaned = doClean ()
    runDotnetTest haveCleaned |> ignore<HaveTested>
| ["-t"; "clean"] -> doClean () |> ignore<HaveCleaned>
| ["-t"; "ci"] ->
    let haveCleaned = doClean ()
    let haveGeneratedAssemblyInfo = generateAssemblyInfo haveCleaned
    let haveBuilt = build haveCleaned haveGeneratedAssemblyInfo
    let haveTested = runDotnetTest haveCleaned
    let haveGeneratedDocs = docs haveBuilt
    runCi haveGeneratedDocs haveTested
| ["-t" ; "ghrelease"] ->
    let haveCleaned = doClean ()
    let haveGeneratedAssemblyInfo = generateAssemblyInfo haveCleaned
    let haveBuilt = build haveCleaned haveGeneratedAssemblyInfo
    let haveTested = runDotnetTest haveCleaned
    gitHubRelease haveTested
| ["-t"; "build"] ->
    let haveCleaned = doClean ()
    let haveGeneratedAssemblyInfo = generateAssemblyInfo haveCleaned
    build haveCleaned haveGeneratedAssemblyInfo |> ignore<HaveBuilt>
| ["-t"; "tests"] ->
    let haveCleaned = doClean ()
    runDotnetTest haveCleaned |> ignore<HaveTested>
| ["-t"; "docs"] ->
    let haveCleaned = doClean ()
    let haveGeneratedAssemblyInfo = generateAssemblyInfo haveCleaned
    let haveBuilt = build haveCleaned haveGeneratedAssemblyInfo
    docs haveBuilt |> ignore<HaveGeneratedDocs>
| ["-t" ; "watchdocs"] -> watchDocs ()
| ["-t"; "releasedocs"] ->
    if isAppVeyorBuild then
        failwith "Refusing to release docs from CI"
    let haveCleaned = doClean ()
    let haveGeneratedAssemblyInfo = generateAssemblyInfo haveCleaned
    let haveBuilt = build haveCleaned haveGeneratedAssemblyInfo
    let haveGeneratedDocs = docs haveBuilt
    releaseDocs haveGeneratedDocs
| ["-t"; "nugetpack"] ->
    let haveCleaned = doClean ()
    let haveTested = runDotnetTest haveCleaned
    packNuGet haveTested |> ignore<HavePacked>
| ["-t"; "nugetpush"] ->
    let haveCleaned = doClean ()
    let haveTested = runDotnetTest haveCleaned
    let havePacked = packNuGet haveTested
    pushNuGet haveTested havePacked |> ignore<HavePushed>
| ["-t"; "buildversion"] ->
    let haveCleaned = doClean ()
    appveyorBuildVersion haveCleaned |> ignore<HaveUpdatedBuildVersion>
| ["-t"; "assemblyinfo"] ->
    let haveCleaned = doClean ()
    generateAssemblyInfo haveCleaned |> ignore<HaveGeneratedAssemblyInfo>
| _ ->
    let args =
        args
        |> String.concat " "
    failwith $"Unrecognised arguments. Supply '-t [RunTests,Clean,CI,Build,Tests,GHRelease,Docs,WatchDocs,ReleaseDocs,NugetPack,NugetPush,BuildVersion,AssemblyInfo]'. Unrecognised args were: %s{args}"

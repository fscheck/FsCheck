// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceBinaries = [ "FsCheck.dll" ]
// Web site location for the generated documentation
let website = "/FsCheck/ja"

let githubLink = "http://github.com/fsharp/FsCheck"

// Specify more information about your project
let info =
  [ "project-name", "FsCheck"
    "project-author", "Kurt Schelfthout and contributors"
    "project-summary", "FsCheck is a tool for testing .NET programs automatically using randomly generated test cases."
    "project-github", githubLink
    "project-nuget", "http://nuget.com/packages/FsCheck" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#load "../../packages/FSharp.Formatting/FSharp.Formatting.fsx"
#r "../../packages/FAKE/tools/NuGet.Core.dll"
#r "../../packages/FAKE/tools/FakeLib.dll"
open Fake
open System.IO
open Fake.FileHelper
open FSharp.Literate
open FSharp.MetadataFormat

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output/ja")
#endif

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../../src/FsCheck.Xunit/bin/Release" //might not work in the future
let content    = __SOURCE_DIRECTORY__ @@ "../content/ja"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let outputJa   = __SOURCE_DIRECTORY__ @@ "../output/ja"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates/ja"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates; formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  CopyRecursive files output true |> Log "Copying file: "
  ensureDirectory (output @@ "content")
  CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
                |> Seq.filter (fun x -> x.Contains "ja")
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    Literate.ProcessDirectory
      ( dir, docTemplate, outputJa @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots, fsiEvaluator = new FsiEvaluator(), lineNumbers=false )

// Generate
copyFiles()
buildDocumentation()

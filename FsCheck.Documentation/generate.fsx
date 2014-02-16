// Given a typical setup (with 'FSharp.Formatting' referenced using NuGet),
// the following will include binaries and load the literate script
#I "../packages/FSharp.Formatting-alpha"
#I @"..\packages\RazorEngine.3.3.0\lib\net40"
#r "FSharp.Literate.dll"
#r "FSharp.Markdown.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "RazorEngine.dll"
open System.IO
open FSharp.Literate

// ----------------------------------------------------------------------------
// SETUP
// ----------------------------------------------------------------------------

/// Return path relative to the current file location
let relative subdir = Path.Combine(__SOURCE_DIRECTORY__, subdir)

let template = relative "templates/template-file.html"

/// Processes a single F# Script file and produce HTML output
let toHtml files =
  files
  |> Seq.iter (fun file ->
    let input = relative (sprintf "%s.fsx" file)
    let output = relative (sprintf "output/%s.html" file)
    printfn "Processing %s into %s" input output
    Literate.ProcessScriptFile(input, template, output, fsiEvaluator=FSharp.Literate.Evaluation.FsiEvaluator(), lineNumbers=false))

toHtml ["QuickStart";"Properties"]


///// Processes an entire directory containing multiple script files 
///// (*.fsx) and Markdown documents (*.md) and it specifies additional 
///// replacements for the template file
//let processDirectory() =
//  let template = relative "/templates/template-project.html"
//  let projInfo =
//    [ "page-description", "F# Literate Programming"
//      "page-author", "Tomas Petricek"
//      "github-link", "https://github.com/tpetricek/FSharp.Formatting"
//      "project-name", "F# Formatting" ]
//  Literate.ProcessDirectory
//    ( __SOURCE_DIRECTORY__, template, dir + "/output-all", 
//      OutputKind.Html, replacements = projInfo)
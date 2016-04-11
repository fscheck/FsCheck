namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FsCheck")>]
[<assembly: AssemblyProductAttribute("FsCheck")>]
[<assembly: AssemblyDescriptionAttribute("FsCheck is a tool for testing .NET programs automatically using randomly generated test cases.")>]
[<assembly: AssemblyVersionAttribute("2.3.1")>]
[<assembly: AssemblyFileVersionAttribute("2.3.1")>]
[<assembly: InternalsVisibleToAttribute("FsCheck.Test")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.3.1"
    let [<Literal>] InformationalVersion = "2.3.1"

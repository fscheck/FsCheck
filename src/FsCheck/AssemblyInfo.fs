namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FsCheck")>]
[<assembly: AssemblyProductAttribute("FsCheck")>]
[<assembly: AssemblyDescriptionAttribute("FsCheck is a tool for testing .NET programs automatically using randomly generated test cases.")>]
[<assembly: AssemblyVersionAttribute("2.0.2")>]
[<assembly: AssemblyFileVersionAttribute("2.0.2")>]
[<assembly: InternalsVisibleToAttribute("FsCheck.Test")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0.2"

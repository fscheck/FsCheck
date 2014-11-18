namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FsCheck")>]
[<assembly: AssemblyProductAttribute("FsCheck")>]
[<assembly: AssemblyDescriptionAttribute("FsCheck is a tool for testing .NET programs automatically using randomly generated test cases.")>]
[<assembly: AssemblyVersionAttribute("1.0.3")>]
[<assembly: AssemblyFileVersionAttribute("1.0.3")>]
[<assembly: InternalsVisibleToAttribute("FsCheck.Test")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.3"

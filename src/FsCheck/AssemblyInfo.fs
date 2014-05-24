namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsCheck")>]
[<assembly: AssemblyProductAttribute("FsCheck")>]
[<assembly: AssemblyDescriptionAttribute("FsCheck is a tool for testing .NET programs automatically using randomly generated test cases.")>]
[<assembly: AssemblyVersionAttribute("0.9.4")>]
[<assembly: AssemblyFileVersionAttribute("0.9.4")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.9.4"

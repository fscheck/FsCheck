namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsCheck.NUnit")>]
[<assembly: AssemblyProductAttribute("FsCheck.NUnit")>]
[<assembly: AssemblyDescriptionAttribute("Integrates FsCheck with NUnit")>]
[<assembly: AssemblyVersionAttribute("2.5.1")>]
[<assembly: AssemblyFileVersionAttribute("2.5.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.5.1"
    let [<Literal>] InformationalVersion = "2.5.1"

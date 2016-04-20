namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyProductAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyDescriptionAttribute("Integrates FsCheck with xUnit.NET")>]
[<assembly: AssemblyVersionAttribute("2.3.1")>]
[<assembly: AssemblyFileVersionAttribute("2.3.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.3.1"
    let [<Literal>] InformationalVersion = "2.3.1"

namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyProductAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyDescriptionAttribute("Integrates FsCheck with xUnit.NET")>]
[<assembly: AssemblyVersionAttribute("3.0.0")>]
[<assembly: AssemblyFileVersionAttribute("3.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "3.0.0"
    let [<Literal>] InformationalVersion = "3.0.0"

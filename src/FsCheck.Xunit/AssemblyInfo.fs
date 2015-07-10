namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyProductAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyDescriptionAttribute("Integrates FsCheck with xUnit.NET")>]
[<assembly: AssemblyVersionAttribute("2.0.3")>]
[<assembly: AssemblyFileVersionAttribute("2.0.3")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0.3"

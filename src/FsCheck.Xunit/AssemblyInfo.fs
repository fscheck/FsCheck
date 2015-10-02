namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyProductAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyDescriptionAttribute("Integrates FsCheck with xUnit.NET")>]
[<assembly: AssemblyVersionAttribute("2.1.1")>]
[<assembly: AssemblyFileVersionAttribute("2.1.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.1.1"

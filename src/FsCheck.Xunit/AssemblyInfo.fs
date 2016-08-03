namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyProductAttribute("FsCheck.Xunit")>]
[<assembly: AssemblyDescriptionAttribute("Integrates FsCheck with xUnit.NET")>]
[<assembly: AssemblyVersionAttribute("2.6.0")>]
[<assembly: AssemblyFileVersionAttribute("2.6.0")>]
[<assembly: InternalsVisibleToAttribute("FsCheck.Test")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.6.0"
    let [<Literal>] InformationalVersion = "2.6.0"

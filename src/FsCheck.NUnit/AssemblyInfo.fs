﻿namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsCheck.Nunit")>]
[<assembly: AssemblyProductAttribute("FsCheck.Nunit")>]
[<assembly: AssemblyDescriptionAttribute("Integrates FsCheck with NUnit")>]
[<assembly: AssemblyVersionAttribute("1.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0"

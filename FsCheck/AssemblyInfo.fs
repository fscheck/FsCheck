namespace FsCheck

open System.Reflection

module private Constant =
    [<Literal>] 
    let version = "0.7.1.0"

[<assembly:AssemblyDescription("FsCheck.dll")>]
[<assembly:AssemblyCompany("FsCheck CodePlex Project")>]
[<assembly:AssemblyTitle("FsCheck.dll")>]
[<assembly:AssemblyProduct("FsCheck random testing framework")>]
[<assembly:AssemblyVersion(Constant.version)>]
[<assembly:AssemblyFileVersion(Constant.version)>]

[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: System.CLSCompliant(true)>]

do()
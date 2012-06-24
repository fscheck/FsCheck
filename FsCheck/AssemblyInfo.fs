namespace FsCheck

open System.Reflection

module private Constant =
    [<Literal>] 
    let version = "0.8.0.0"

[<assembly:AssemblyDescription("FsCheck.dll")>]
[<assembly:AssemblyCompany("FsCheck CodePlex Project")>]
[<assembly:AssemblyTitle("FsCheck.dll")>]
[<assembly:AssemblyProduct("FsCheck random testing framework")>]
[<assembly:AssemblyVersion(Constant.version)>]
[<assembly:AssemblyFileVersion(Constant.version)>]

[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: System.CLSCompliant(true)>]
[<assembly: System.Runtime.CompilerServices.InternalsVisibleToAttribute("FsCheck.Test, PublicKey=0024000004800000940000000602000000240000525341310004000001000100258572dc89a39b3bdd0ab47dcc75150c50699ca24c443c8ebbfc908f6a368153f3c5db12908f2afaa183b16a46f6d64693391bdd86757223126f5a4b69fb62f7f1d0fea98ba273fd0fa03f45852c55b419ac392942d117e1c4d1657dd83919fb462c4d97b2038dcbef3581033c1f167f9b8700b485079af78709517fc8f72df2")>]

do()
namespace FsCheck.NUnit.Examples


open FsCheck.NUnit

//[<NUnitAddin(Description = "FsCheck addin")>]
//type FsCheckAddin() =
//    interface IAddin with
//        override x.Install host =
//            let tcBuilder = new FsCheckTestCaseBuilder()
//            host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
//            true
//
//

namespace FsCheck.NUnit.Addin

open NUnit.Core
open NUnit.Core.Extensibility

open FsCheck.NUnit

//type FsCheckTestCaseBuilder() =        
//    interface ITestCaseBuilder with
//        override x.CanBuildFrom mi = 
//            Reflect.HasAttribute(mi, typeof<PropertyAttribute>.FullName, false)
//        override x.BuildFrom mi =                         
//            FsCheckTestMethod(mi) :> Test
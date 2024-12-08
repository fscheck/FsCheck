namespace Fscheck.Test.FsCheck.NUnit.PropertyAttribute

open FsCheck
open FsCheck.NUnit

module ResultStateExceptionHandlingTest =
    [<Property>]
    let ``should pass when AssertPass called``() =
        NUnit.Framework.Assert.Pass()

    [<Property>]
    let ``should pass when AssertPass called inside async``() =
        async {
            NUnit.Framework.Assert.Pass()
        }


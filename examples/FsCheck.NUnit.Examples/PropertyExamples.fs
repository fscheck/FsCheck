namespace FsCheck.NUnit.Examples

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

[<TestFixture>]
type NUnitTest() =

    [<Test>]
    member __.NormalTest() =
        ignore true

    [<Property>]
    member __.RevUnit (x:char) =
        List.rev [x] = [x]
  
    [<Property>]
    member __.RevApp (x:string) xs =
        List.rev (x::xs) = List.rev xs @ [x]
            |> Prop.trivial (xs = [])
            |> Prop.trivial (xs.Length = 1)

    [<Property>]
    member __.MaxLe (x:float) y =
        (x <= y) ==> (lazy (max  x y = y))

    [<Property( Replay="54321,67584", Verbose = true )>]
    member __.Replay x =
        System.Int32.MaxValue >= x

    [<Property( Replay="lame,string" )>]
    member __.ReplayBroken_shouldFail (x:float) y =
        true

    [<Property>]
    member __.PrintUnhandledException_ShouldFail (xs : int list) =
        Assert.IsTrue(List.length xs < 6, "this message should be visible in test explorer")

    [<Property; Ignore("reason")>]
    member __.ShouldIgnore (xs : int list) =
        false

    [<Property; Category("foobar")>]
    member __.ShouldApplyCategory (xs : int) =
        true

    // Note: should fail
    [<Property( Verbose = true )>]
    member __.RevIdVerbose_shouldFail (xs:int[]) =
        Array.rev xs = xs

    // Note: should fail, this test property does not make sense. it is used to verify the exhausted case fails this nunit test
    [<Property( MaxTest = 1 )>]
    member __.Exhausted_shouldFail (x:float) =
        false ==> (x > 0.)

    [<Property>]
    member __.Product (x:int, y:int) =
        (x > 0 && y > 0) ==> (x*y > 0)

    [<Property(QuietOnSuccess = true)>]
    member __.NoOutputOnSuccess (x:char) =
        List.rev [x] = [x]
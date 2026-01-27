namespace FsCheck.NUnit.Examples

open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open FsCheck.NUnit
open FsCheck.Fluent

[<TestFixture>]
type NUnitTest() =

    [<Test>]
    member __.NormalTest() =
        ignore true
        
    [<Property>]
    member __.RevUnit (x:char) =
        List.rev [x] = [x]

    [<Property(Parallelism = 8)>]
    member __.Async (i:int) =
        async { 
            do! Async.Sleep 1500
            return true
        }
  
    [<Property>]
    member __.RevApp (x:string) xs =
        List.rev (x::xs) = List.rev xs @ [x]
            |> Prop.trivial (xs = [])
            |> Prop.trivial (xs.Length = 1)

    [<Property>]
    member __.MaxLe (x:float) y =
        (x <= y) ==> (lazy (max  x y = y))

    [<Property( Replay="54321,67585", Verbose = true )>]
    member __.Replay x =
        System.Int32.MaxValue >= x

    [<Property( Replay="lame,string" )>]
    member __.ReplayBroken_shouldFail (x:float) y =
        true

    [<Property>]
    member __.PrintUnhandledException_ShouldFail (xs : int list) =
        Assert.That(List.length xs < 6, "this message should be visible in test explorer")

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

    // Regression test for issue: Implies with Replay causes TargetInvocationException/NullReferenceException
    // This test verifies that using Implies with Replay doesn't throw a NullReferenceException
    // Note: This test will be exhausted because the condition is always false, but that's expected behavior
    [<Property(Replay="123,127,123")>]
    member __.ImpliesWithReplayFalseCondition_shouldFail() =
        // This will result in "Exhausted" because the condition is always false,
        // but it should NOT throw a NullReferenceException (which was the bug)
        false.Implies(true)

    // Regression test with a passing condition
    [<Property(Replay="123,127,123")>]
    member __.ImpliesWithReplayTrueCondition() =
        // This should pass because the condition is true and the property is true
        true.Implies(true)
[<NUnit.Framework.TestFixture>]
module PropertyExamples

    open NUnit.Framework
    open FsCheck
    open FsCheck.NUnit

    [<Property>]
    let revUnit (x:char) =
        List.rev [x] = [x]

    [<Property>]
    let revApp (x:string) xs =
        List.rev (x::xs) = List.rev xs @ [x]
            |> Prop.trivial (xs = [])
            |> Prop.trivial (xs.Length = 1)

    [<Property>]
    let maxLe (x:float) y =
        (x <= y) ==> (lazy (max  x y = y))

    // Note: should fail
    [<Property( Verbose = true )>]
    let revIdVerbose_shouldFail (xs:int[]) =
        Array.rev xs = xs

    // Note: should fail, this test property does not make sense. it is used to verify the exhausted case fails this nunit test
    [<Property( MaxTest = 1 )>]
    let exhausted_shouldFail (x:float) =
        false ==> (x > 0.)

    [<Property>]
    let product (x:int, y:int) =
        (x > 0 && y > 0) ==> (x*y > 0)

    [<Property(QuietOnSuccess = true)>]
    let noOutputOnSuccess (x:char) =
        List.rev [x] = [x]
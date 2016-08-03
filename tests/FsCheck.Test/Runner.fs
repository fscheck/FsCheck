﻿namespace FsCheck.Test

module Runner =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Swensen.Unquote

    type TestArbitrary1 =
        static member PositiveDouble() =
            Arb.Default.Float()
            |> Arb.mapFilter abs (fun t -> t >= 0.0)

    type TestArbitrary2 =
        static member NegativeDouble() =
            Arb.Default.Float()
            |> Arb.mapFilter (abs >> ((-) 0.0)) (fun t -> t <= 0.0)

    [<Property( Arbitrary=[| typeof<TestArbitrary2>; typeof<TestArbitrary1> |] )>]
    let ``should register Arbitrary instances from Config in last to first order``(underTest:float) =
        underTest <= 0.0

    [<Fact>]
    let ``should discard case with discardexception in gen``() =
        let myGen = 
            gen {
                let! a = Gen.choose(0, 4)
                return if a > 3 
                            then Prop.discard()
                            else a
            }

        let myArb = Arb.fromGen myGen
        
        Check.QuickThrowOnFailure <| Prop.forAll myArb (fun a -> a <= 3)

    [<Fact>]
    let ``should discard case with discardexception in test``() =
        Check.QuickThrowOnFailure <| (fun a -> if a > 3 then Prop.discard() else true)
        
    [<Fact>]
    let ``should replay property with one generator``() =
        let doOne(s1,s2) =
            try
                Check.One( {Config.QuickThrowOnFailure with Replay = Some <| Random.StdGen (s1,s2) }, fun a -> a < 5)
                "should have failed"
            with e ->
                e.Message
        let same =
            Seq.initInfinite (fun _ -> doOne(123,654321))
            |> Seq.take(5)
            |> Seq.distinct
        1 =! Seq.length same
        "should have failed" <>! Seq.head same
        test <@ (Seq.head same).Contains "(123,654321)" @>

    [<Fact>]
    let ``should replay property with complex set of generators``() =
        let doOne(s1,s2) =
            try
                Check.One( {Config.QuickThrowOnFailure with Replay = Some <| Random.StdGen (s1,s2) }, fun a (_:list<char>, _:array<int*double>) (_:DateTime) -> a < 10)
                "should have failed"
            with e ->
                e.Message
        let same =
            Seq.initInfinite (fun _ -> doOne(123,654321))
            |> Seq.take(5)
            |> Seq.distinct
        1 =! Seq.length same
        "should have failed" <>! Seq.head same
        test <@ (Seq.head same).Contains "(123,654321)" @>

    [<Property(Replay="54321,67584")>]
    let ``should pick up replay seeds from PropertyAttribute without parens``(_:int, _:string) =
        //testing the replay separately in other tests - this just checks we can run
        //this test
        ()

    [<Property(Replay="(54321,67584)")>]
    let ``should pick up replay seeds from PropertyAttribute with parens``(_:int, _:string) =
        //testing the replay separately in other tests - this just checks we can run
        //this test
        ()

    type TypeToInstantiate() =
        [<Property>]
        member __.``Should run a property on an instance``(_:int) = ()

    [<Arbitrary(typeof<TestArbitrary2>)>]
    module ModuleWithArbitrary =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0

    [<Properties(Arbitrary = [| typeof<TestArbitrary2> |])>]
    module ModuleWithProperties =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0

module BugReproIssue195 =

    open FsCheck
    open FsCheck.Xunit
    open System

    let intStr = Arb.Default.Int32() |> Arb.toGen |> Gen.map string

    // since this used to go via from<string>, it memoized the string generator, meaning at
    // registration time for the string generator below, a duplicate key exception was thrown.
    // this was fixed by using Default.String() in StringWithoutNullChars instead, and a bunch
    // of other similar cases in the default generator was fixed as well.
    let breaksIt = Arb.Default.StringWithoutNullChars() |> ignore

    type BrokenGen = 
        static member String() = intStr |> Arb.fromGen

    [<Property(Arbitrary = [| typeof<BrokenGen> |])>]
    let ``broken`` (s : String) = s |> ignore
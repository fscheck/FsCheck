﻿namespace FsCheck.Test

module Runner =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Helpers

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
        //let myArb = Arb.Default.Int32() |> Arb.filter (fun a -> a < 4)

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
            Seq.initInfinite (fun i -> doOne(123,654321))
            |> Seq.take(5)
            |> Seq.distinct
        Assert.Equal(1,Seq.length same)
        Assert.NotEqual<string>("should have failed", Seq.head same)
        Assert.Contains("(123,654321)", Seq.head same);

    [<Fact>]
    let ``should replay property with complex set of generators``() =
        let doOne(s1,s2) =
            try
                Check.One( {Config.QuickThrowOnFailure with Replay = Some <| Random.StdGen (s1,s2) }, fun a (b:list<char>, c:array<int*double>) (d:DateTime) -> a < 10)
                "should have failed"
            with e ->
                e.Message
        let same =
            Seq.initInfinite (fun i -> doOne(123,654321))
            |> Seq.take(5)
            |> Seq.distinct
        Assert.Equal(1,Seq.length same)
        Assert.NotEqual<string>("should have failed", Seq.head same)
        Assert.Contains("(123,654321)", Seq.head same);

    [<Property(Replay="54321,67584")>]
    let ``should pick up replay seeds from PropertyAttribute without parens``(a:int, b:string) =
        //testing the replay separately in other tests - this just checks we can run
        //this test
        Assert.True true

    [<Property(Replay="(54321,67584)")>]
    let ``should pick up replay seeds from PropertyAttribute with parens``(a:int, b:string) =
        //testing the replay separately in other tests - this just checks we can run
        //this test
        Assert.True true

    type TypeToInstantiate() =
        [<Property>]
        let ``should run a property on an instance``(random:int) =
            Assert.True true

    [<Arbitrary(typeof<TestArbitrary2>)>]
    module ModuleWithArbitrary =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0

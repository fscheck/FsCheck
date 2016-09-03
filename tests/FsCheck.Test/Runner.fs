namespace FsCheck.Test

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

    [<Fact>]
    let ``PropertyConfig combine should prepend extra Arbitrary``() =
        let original = { PropertyConfig.zero with Arbitrary = [| typeof<TestArbitrary1> |] }
        let extra    = { PropertyConfig.zero with Arbitrary = [| typeof<TestArbitrary2> |] }
        let combined = PropertyConfig.combine extra original

        combined.Arbitrary.[0] =! typeof<TestArbitrary2>

    [<Property>]
    let ``PropertyConfig combine should favor extra config``(orignalMaxTest, extraMaxTest) =
        let original = { PropertyConfig.zero with MaxTest = Some orignalMaxTest }
        let extra    = { PropertyConfig.zero with MaxTest = Some extraMaxTest }
        let combined = PropertyConfig.combine extra original

        combined.MaxTest =! Some extraMaxTest

    [<Property>]
    let ``PropertyConfig toConfig should favor specified setting``(maxTest) =
        let propertyConfig = { PropertyConfig.zero with MaxTest = Some maxTest }
        let testOutputHelper = new Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.MaxTest =! maxTest

    [<Fact>]
    let ``PropertyConfig toConfig should use defaults as a fallback``() =
        let propertyConfig = PropertyConfig.zero
        let testOutputHelper = new Sdk.TestOutputHelper()
        let config = PropertyConfig.toConfig testOutputHelper propertyConfig

        config.MaxTest =! Config.Default.MaxTest

    type TypeToInstantiate() =
        [<Property>]
        member __.``Should run a property on an instance``(_:int) = ()

    [<Properties(Arbitrary = [| typeof<TestArbitrary2> |])>]
    module ModuleWithPropertiesArb =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0

    [<Properties( MaxTest = 1, StartSize = 100, EndSize = 100, Replay = "01234,56789")>]
    module ModuleWithPropertiesConfig =

        [<Property>]
        let ``should use configuration from enclosing module``(x:int) =
            // checking if the generated value is always the same (-59) from "01234,56789" Replay
            x =! -59

        [<Property( Replay = "12345,67890")>]
        let ``should use configuration on method preferentially``(x:int) =
            // checking if the generated value is always the same (18) from "12345,67890" Replay
            x =! 18

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

// Compiler warning FS0044 occurs when a construct is deprecated.
// This warning suppression has to sit in the end of the file, because once a
// warning type is suppressed in a file, it can't be turned back on. There's a
// feature request for that, though: 
// https://fslang.uservoice.com/forums/245727-f-language/suggestions/6085102-allow-f-compiler-directives-like-nowarn-to-span
#nowarn"44"

module Deprecated =

    open FsCheck.Xunit
    open Runner
    
    [<Arbitrary(typeof<TestArbitrary2>)>]
    module ModuleWithArbitrary =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0
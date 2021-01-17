namespace FsCheck


//private interface for reflection
type internal IArbitrary =
    abstract GeneratorObj: Gen<obj>
    abstract ShrinkerObj: obj -> seq<obj>

[<AbstractClass>]
type Arbitrary<'T>() =
    ///Returns a generator for 'a.
    abstract Generator: Gen<'T>
    ///Returns a sequence of the immediate shrinks of the given value. The immediate shrinks should not include
    ///doubles or the given value itself. The default implementation returns the empty sequence (i.e. no shrinking).
    abstract Shrinker: 'T -> seq<'T>
    default _.Shrinker _ = Seq.empty

    interface IArbitrary with
        member x.GeneratorObj = (x.Generator :> IGen).AsGenObject

        member x.ShrinkerObj(o: obj): seq<obj> =
            try
                unbox o |> x.Shrinker |> Seq.map box
            with _ -> Seq.empty

namespace FsCheck.FSharp

open FsCheck

module Arb =

    let toGen (arb: Arbitrary<'Value>) = arb.Generator

    let toShrink (arb: Arbitrary<'Value>) = arb.Shrinker

    /// Construct an Arbitrary instance from a generator.
    /// Shrink is not supported for this type.
    [<CompiledName("From")>]
    let fromGen (gen: Gen<'Value>): Arbitrary<'Value> =
        { new Arbitrary<'Value>() with
            override __.Generator = gen }

    /// Construct an Arbitrary instance from a generator and shrinker.
    [<CompiledName("From")>]
    let fromGenShrink (gen: Gen<'Value>, shrinker: 'Value -> seq<'Value>): Arbitrary<'Value> =
        { new Arbitrary<'Value>() with
            override __.Generator = gen
            override __.Shrinker a = shrinker a }

    ///Construct an Arbitrary instance for a type that can be mapped to and from another type (e.g. a wrapper),
    ///based on a Arbitrary instance for the source type and two mapping functions.
    [<CompiledName("Convert")>]
    let convert convertTo convertFrom (a: Arbitrary<'T>) =
        { new Arbitrary<'U>() with
            override __.Generator = a.Generator |> Gen.map convertTo
            override __.Shrinker b =
                b
                |> convertFrom
                |> a.Shrinker
                |> Seq.map convertTo }

    /// Return an Arbitrary instance that is a filtered version of an existing arbitrary instance.
    /// The generator uses Gen.suchThat, and the shrinks are filtered using Seq.filter with the given predicate.
    [<CompiledName("Filter")>]
    let filter pred (a: Arbitrary<'T>) =
        { new Arbitrary<'T>() with
            override __.Generator = a.Generator |> Gen.where pred
            override __.Shrinker b = b |> a.Shrinker |> Seq.filter pred }

    /// Return an Arbitrary instance that is a mapped and filtered version of an existing arbitrary instance.
    /// The generator uses Gen.map with the given mapper and then Gen.suchThat with the given predicate,
    /// and the shrinks are filtered using Seq.filter with the given predicate.
    ///This is sometimes useful if using just a filter would reduce the chance of getting a good value
    ///from the generator - and you can map the value instead. E.g. PositiveInt.
    [<CompiledName("MapFilter")>]
    let mapFilter mapper pred (a: Arbitrary<'T>) =
        { new Arbitrary<'T>() with
            override __.Generator =
                a.Generator |> Gen.map mapper |> Gen.where pred
            override __.Shrinker b =
                b |> a.Shrinker |> Seq.filter pred }

  
 
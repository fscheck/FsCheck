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
open System
open FsCheck.Internals

[<RequireQualifiedAccess>]
module Arb =

    /// Get the Gen from the given Arbitary.
    let toGen (arb: Arbitrary<'Value>) = arb.Generator

    /// Get the shrinker function from the given Arbitrary.
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

    /// Construct an Arbitrary instance for a type that can be mapped to and from another type (e.g. a wrapper),
    /// based on a Arbitrary instance for the source type and two mapping functions.
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
    /// This is sometimes useful if using just a filter would reduce the chance of getting a good value
    /// from the generator - and you can map the value instead. E.g. PositiveInt.
    [<CompiledName("MapFilter")>]
    let mapFilter mapper pred (a: Arbitrary<'T>) =
        { new Arbitrary<'T>() with
            override __.Generator =
                a.Generator |> Gen.map mapper |> Gen.where pred
            override __.Shrinker b =
                b |> a.Shrinker |> Seq.filter pred }
        
        
    let zip(t1: Arbitrary<'T1>, t2: Arbitrary<'T2>) =
        let generator = Gen.zip t1.Generator t2.Generator
        let shrinker (l,r) = Seq.zip (t1.Shrinker l) (t2.Shrinker r)
        fromGenShrink(generator, shrinker)

    /// Generates one-dimensional arrays. 
    /// The length of the generated array is between 0 and size.
    /// The sum of the sizes of the elements is equal to the size of the generated array.
    let array (elements: Arbitrary<'T>) =
        let generator = Gen.arrayOf elements.Generator
        fromGenShrink(generator, Shrink.array elements.Shrinker)

    /// Generates option values that are None 1/8 of the time.
    let option (value: Arbitrary<'T>) = 
        let generator = Gen.optionOf value.Generator
        let shrinker o =
            match o with
            | Some x -> seq { yield None; for x' in value.Shrinker x -> Some x' }
            | None  -> Seq.empty
        fromGenShrink(generator, shrinker)

    /// Generates nullable values that are null 1/8 of the time.
    let nullable (value: Arbitrary<'T>) = 
        let generator = Gen.frequency [(1, Gen.fresh Nullable); (7, Gen.map Nullable value.Generator)]
        let shrinker (o:Nullable<_>) =
            if o.HasValue then
                seq { yield Nullable(); for x' in value.Shrinker o.Value -> Nullable(x') }
            else 
                Seq.empty
        fromGenShrink(generator, shrinker)

    /// Generates FSharp.Core list<'T> values. 
    /// The length of the generated list is between 0 and size. 
    /// The sum of the sizes of the elements is equal to the size of the generated list.
    let list (elements: Arbitrary<'T>) = 
        let generator = Gen.listOf elements.Generator
        fromGenShrink(generator, Shrink.list elements.Shrinker)

    /// Generates pure functions that produce the given output values 'U. 
    /// There is no shrinking for functions.
    let pureFunction (target:Arbitrary<'U>) :Arbitrary<'T->'U> = 
        Gen.pureFunction target.Generator
        |> fromGen

    /// Generates F# function values that generate an instance of the function result type about half the time. The other 
    /// times it generate one of the given exceptions.
    let throwingFunction<'T,'U when 'T:equality> (exceptions:seq<Exception>) (target: Arbitrary<'U>) :Arbitrary<'T->'U> = 
        let exc = exceptions |> Seq.toArray
        let throwExc = Gen.elements exc |> Gen.map raise
        let gen = Gen.frequency [ (exc.Length, target.Generator)
                                  (exc.Length, throwExc) ]
                  |> Gen.pureFunction
        fromGen gen
    
    /// Generates Set<'T> values.
    let set (elements: Arbitrary<'T>) = 
        list elements
        |> convert Set.ofList Set.toList

     /// Generates Map<TKey,TValue> values.
     /// Not named `map` because that has other meanings.
    let mapKV (keys: Arbitrary<'TKey>, values: Arbitrary<'TValue>) = 
        zip (keys, values)
        |> list
        |> convert Map.ofList Map.toList
        
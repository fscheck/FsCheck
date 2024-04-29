namespace FsCheck.Fluent

open System
open System.Runtime.CompilerServices

open FsCheck
open FsCheck.FSharp


[<Extension;Sealed;AbstractClass>]
type Arb private() =

    /// Construct an Arbitrary instance from a generator.
    /// Shrink is not supported for this type.
    static member From(gen: Gen<'Value>) : Arbitrary<'Value> =
        Arb.fromGen(gen)

    /// Construct an Arbitrary instance from a generator and shrinker.
    static member From(gen: Gen<'Value>, shrinker: Func<'Value, seq<'Value>>): Arbitrary<'Value> =
       Arb.fromGenShrink(gen, shrinker.Invoke)

    /// Construct an Arbitrary instance from a generator.
    /// Shrink is not supported for this type.
    [<Extension>]
    static member ToArbitrary(generator) :Arbitrary<'T> =
        Arb.fromGen generator

    /// Construct an Arbitrary instance from a generator and shrinker.
    static member ToArbitrary(gen: Gen<'Value>, shrinker: Func<'Value, seq<'Value>>): Arbitrary<'Value> =
        Arb.fromGenShrink(gen, shrinker.Invoke)

    ///Construct an Arbitrary instance for a type that can be mapped to and from another type (e.g. a wrapper),
    ///based on a Arbitrary instance for the source type and two mapping functions. 
    [<Extension>]
    static member Convert (arb:Arbitrary<'T>, convertTo: Func<_,_>, convertFrom: Func<_,_>) :Arbitrary<'U> =
        Arb.convert convertTo.Invoke convertFrom.Invoke arb

    /// Return an Arbitrary instance that is a filtered version of an existing arbitrary instance.
    /// The generator uses Gen.where, and the shrinks are filtered using Seq.filter with the given predicate.
    [<Extension>]
    static member Filter (arb:Arbitrary<'T>, filter: Func<_,_>) =
        Arb.filter filter.Invoke arb

    /// Return an Arbitrary instance that is a mapped and filtered version of an existing arbitrary instance.
    /// The generator uses Gen.map with the given mapper and then Gen.where with the given predicate, 
    /// and the shrinks are filtered using Seq.filter with the given predicate.
    ///This is sometimes useful if using just a filter would reduce the chance of getting a good value
    ///from the generator - and you can map the value instead. E.g. PositiveInt.
    [<Extension>]
    static member MapFilter (arb:Arbitrary<'T>, map: Func<_,_>, filter: Func<_,_>) =
        Arb.mapFilter map.Invoke filter.Invoke arb

    /// Generates 2-tuples.
    [<Extension>]
    static member Zip(t1: Arbitrary<'T1>, t2: Arbitrary<'T2>) =
        let generator = Gen.Zip(t1.Generator, t2.Generator)
        let shrinker (struct (l,r)) = Seq.map2 (fun l r -> struct (l,r)) (t1.Shrinker l) (t2.Shrinker r)
        Arb.fromGenShrink(generator, shrinker)

    /// Generates one-dimensional arrays. 
    /// The length of the generated array is between 0 and size.
    /// The sum of the sizes of the elements is equal to the size of the generated array.
    [<Extension>]
    static member Array (elements: Arbitrary<'T>) =
        Arb.array elements

    /// Generates nullable values that are null 1/8 of the time.
    [<Extension>]
    static member Nullable (value: Arbitrary<'T>) = 
        Arb.nullable value



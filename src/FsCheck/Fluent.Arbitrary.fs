namespace FsCheck.Fluent

open System
open System.Runtime.CompilerServices

open FsCheck
open FsCheck.Internals
open FsCheck.FSharp


[<Extension;Sealed;AbstractClass>]
type Arb private() =

    /// Convert a generator to an arbitrary without shrinking.
    static member From(generator:Gen<'T>) =
        Arb.fromGen generator

    /// Convert an arbitrary while preserving its shrink tree.
    [<Extension>]
    static member Convert(arb:Arbitrary<'T>, convertTo:Func<'T,'U>, _convertFrom:Func<'U,'T>) =
        if isNull convertTo then nullArg "convertTo"
        if isNull _convertFrom then nullArg "convertFrom"
        Arb.map convertTo.Invoke arb

    
    [<Extension>]
    static member Select (arb:Arbitrary<'T>, select: Func<_,_>) :Arbitrary<'U> =
        Arb.map select.Invoke arb

    /// Return an Arbitrary instance that is a filtered version of an existing arbitrary instance.
    /// The generator uses Gen.where, and the shrinks are filtered using Seq.filter with the given predicate.
    [<Extension>]
    static member Where (arb:Arbitrary<'T>, filter: Func<_,_>) =
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
        Arb.map2 (fun left right -> struct (left, right)) t1 t2

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

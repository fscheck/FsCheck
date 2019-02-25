namespace FsCheck

open System

///Extensions to transform Arbitrary instances into other Arbitrary instances.
[<System.Runtime.CompilerServices.Extension>]
type ArbitraryExtensions =
    ///Construct an Arbitrary instance for a type that can be mapped to and from another type (e.g. a wrapper),
    ///based on a Arbitrary instance for the source type and two mapping functions. 
    [<System.Runtime.CompilerServices.Extension>]
    static member Convert (arb, convertTo: Func<_,_>, convertFrom: Func<_,_>) =
        Arb.convert convertTo.Invoke convertFrom.Invoke arb

    /// Return an Arbitrary instance that is a filtered version of an existing arbitrary instance.
    /// The generator uses Gen.suchThat, and the shrinks are filtered using Seq.filter with the given predicate.
    [<System.Runtime.CompilerServices.Extension>]
    static member Filter (arb, filter: Func<_,_>) =
        Arb.filter filter.Invoke arb

    /// Return an Arbitrary instance that is a mapped and filtered version of an existing arbitrary instance.
    /// The generator uses Gen.map with the given mapper and then Gen.suchThat with the given predicate, 
    /// and the shrinks are filtered using Seq.filter with the given predicate.
    ///This is sometimes useful if using just a filter would reduce the chance of getting a good value
    ///from the generator - and you can map the value instead. E.g. PositiveInt.
    [<System.Runtime.CompilerServices.Extension>]
    static member MapFilter (arb, map: Func<_,_>, filter: Func<_,_>) =
        Arb.mapFilter map.Invoke filter.Invoke arb



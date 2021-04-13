namespace FsCheck

open FsCheck.Internals

type internal IArbitrary =
    abstract ArbitraryObj: Arbitrary<obj>

and Arbitrary<'T> = private Arbitrary of Gen<Shrink<'T>> with
    interface IArbitrary with
        member t.ArbitraryObj =
            let (Arbitrary genShrink) = t
            genShrink |> FSharp.Gen.map (Shrink.map unbox) |> Arbitrary

namespace FsCheck.FSharp

open System
open System.Collections.Generic

open FsCheck
open FsCheck.Internals

[<RequireQualifiedAccess>]
module Arb =
 
    let internal unArb (Arbitrary g) = g

    /// Generates n values of the given size and starting with the given seed.
    let sampleWithSeed seed size nbSamples (Arbitrary generator) :'T[] = 
        generator
        |> Gen.sampleWithSeed seed size nbSamples
        |> Array.map (Shrink.getValue >> fst)

    /// Generates a given number of values with a new seed and a given size.
    let sampleWithSize size nbSamples gen : 'T[] = sampleWithSeed (Random.Create()) size nbSamples gen

    /// Generates a given number of values with a new seed and a size of 50.
    let sample nbSamples gen : 'T[] = sampleWithSize 50 nbSamples gen

    /// Generates one sample from the given Arbitrary, and returns a sequence
    /// of the generated value and its immediate shrink attempts.
    let shrinkSample (Arbitrary arb) =
        arb
        |> Gen.sample 1
        |> Array.head
        |> Shrink.getValue
        |> fun (generated,shrinks) -> generated, seq {
            for elem in shrinks do
                yield elem |> Shrink.getValue |> fst
        }

    /// Generates only the given constant value.
    /// Does not shrink (because there is only one value).
    let constant (value:'T) =
        value
        |> Shrink.ofValue
        |> Gen.constant
        |> Arbitrary

    /// Generates int values in the given range (inclusive).
    /// Shrinks towards zero, or as close as it can get within
    /// the given range.
    let rec choose (l,h) =
        if l > h then
            choose (h,l)
        else
            Gen.choose (l,h)
            |> Gen.map (Shrink.ofShrinker (Shrink.signedNumberBetween l h) id)
            |> Arbitrary

    /// Generates int64 values in the given range (inclusive).
    /// Shrinks towards zero, or as close as it can get within
    /// the given range.
    let rec choose64 (l,h) = 
        if l > h then
            choose64 (h,l)
        else
            Gen.choose64 (l,h)
            |> Gen.map (Shrink.ofShrinker (Shrink.signedNumberBetween l h) id)
            |> Arbitrary

    let sized (sizeFun: int -> Arbitrary<'T>) :Arbitrary<'T> =
        Gen.sized (sizeFun >> unArb)
        |> Arbitrary

    let resize (size: int) (Arbitrary arb: Arbitrary<'T>) :Arbitrary<'T> =
        Gen.resize size arb
        |> Arbitrary

    let map (f:'T -> 'U) (Arbitrary arb) =
        arb
        |> Gen.map (Shrink.map f)
        |> Arbitrary

    /// Return an Arbitrary instance that is a filtered version of an existing arbitrary instance.
    /// The generator uses Gen.suchThat, and the shrinks are filtered using Seq.filter with the given predicate.
    let filter (predicate:'T -> bool) (Arbitrary arb) =
        arb
        |> Gen.filter (Shrink.getValue >> fst >> predicate)
        |> Gen.map (Shrink.filter predicate)
        |> Arbitrary
    
    let map2 (f: 'T1 -> 'T2 -> 'U) (Arbitrary arb1) (Arbitrary arb2) =
        Gen.map2 (Shrink.map2 f) arb1 arb2
        |> Arbitrary
      
    let zip (t1: Arbitrary<'T1>) (t2: Arbitrary<'T2>) =
        map2 (fun t1 t2 -> (t1,t2)) t1 t2

    let apply (f: Arbitrary<'T->'U>) (arb: Arbitrary<'T>) =
        map2 (fun f a -> f a) f arb

    let private bindGenShrink f (Gen gen) =
        (fun s r0 ->
            let struct(shrinkT,r1) = gen s r0
            // r2Store is used to conveniently pass through the resulting seed.
            let mutable r2Store = Rnd()
            let shrinkU = 
                shrinkT
                |> Shrink.bind (fun v ->
                    let (Arbitrary (Gen mb)) = f v
                    // because r1 is captured here, we should see
                    // similar values being generated from mb during shrinking
                    // provided the generators retured by f are similar.
                    let struct(b,r2) = mb s r1
                    r2Store <- r2
                    b)
            struct(shrinkU, r2Store))
        |> Gen

    let bind (f: 'T -> Arbitrary<'U>) (Arbitrary gen: Arbitrary<'T>) =
        bindGenShrink f gen
        |> Arbitrary

    let elements (xs : seq<'T>) =
        let elems = ListAccessWrapper<_>.Create xs
        choose (0, elems.Count-1)
        |> map elems.Item

    let oneof (gens:seq<Arbitrary<'T>>) = bind id (elements gens)

    let frequency (dist:seq<int*Arbitrary<'T>>) =
        let tot = Seq.sumBy fst dist
        if tot <= 0 then 
            invalidArg "dist" "frequency was called with a sum of probabilities less than or equal to 0. no elements can be generated."
        let xs = ListAccessWrapper<_>.Create dist
        let rec pick i n =
            let weight,_ = xs.[i]
            if n<weight then i else pick (i+1) (n-weight)
        
        // this is not written in terms of choose directly, because then shrinking doesn't work as well:
        // If we choose the weight, then shrinks of that weight can map to the same arbitrary instance in the dist
        // sequence, and so we'll try the same shrinks multiple times.
        // However, in this was, we first map the weight to an index in the list, and then the index is shrunk
        // as normal, so each arbitrary in the list is only tried once.
        Gen.choose (0,tot-1)
        |> Gen.map (pick 0 >> fun i -> Shrink.ofShrinker (Shrink.signedNumberBetween 0 i) id i)
        |> bindGenShrink (fun idx ->  xs.[idx] |> snd)
        |> Arbitrary 

    let collectToList (f: 'T -> Arbitrary<'U>) (source:seq<'T>) =
        source
        |> Gen.collectToList (f >> unArb)
        |> Gen.map (Shrink.sequenceToList)
        |> Arbitrary

    let sequenceToList (source:seq<Arbitrary<'T>>) =
        collectToList id source

    let collectToArray (f: 'T -> Arbitrary<'U>) (source:seq<'T>) =
        source
        |> Gen.collectToArray (f >> unArb)
        |> Gen.map (Shrink.sequenceToArray)
        |> Arbitrary

    
    let sequenceToArray (source:seq<Arbitrary<'T>>) =
        collectToArray id source

    let bool = elements [false; true]

    let double maxAbsValue =
        Gen.map2 (fun f isNegative -> 
            let value = f * float maxAbsValue
            if isNegative then -value else value)
            Gen.double Gen.bool
        |> Gen.map (Shrink.ofShrinker Shrink.double id)
        |> Arbitrary

    /// Generates option values that are None 1/8 of the time.
    let option (value: Arbitrary<'T>) = 
        frequency [(1, constant None); (7, map Some value)]

    /// Generates nullable values that are null 1/8 of the time.
    let nullable (value: Arbitrary<'T>) = 
        frequency [(1, constant (Nullable())); (7, map Nullable value)]

    /// Generates FSharp.Core list<'T> values. 
    /// The length of the generated list is between 0 and size. 
    /// The sum of the sizes of the elements is equal to the size of the generated list.
    let list (elements: Arbitrary<'T>) =
        elements
        |> unArb
        |> Gen.listOf
        |> Gen.map (Shrink.sequenceToList >> Shrink.bindInner (Shrink.ofShrinker Shrink.listShorten id))
        |> Arbitrary

    /// Generates one-dimensional arrays. 
    /// The length of the generated array is between 0 and size.
    /// The sum of the sizes of the elements is equal to the size of the generated array.
    let array (elements: Arbitrary<'T>) =
        elements
        |> unArb
        |> Gen.arrayOf
        |> Gen.map (Shrink.sequenceToArray >> Shrink.bindInner (Shrink.ofShrinker Shrink.arrayShorten id))
        |> Arbitrary

    ///// Generates pure functions that produce the given output values 'U. 
    ///// There is no shrinking for functions.
    //let pureFunction (target:Arbitrary<'U>) :Arbitrary<'T->'U> = 
    //    Gen.pureFunction target.Generator
    //    |> fromGen

    ///// Generates F# function values that generate an instance of the function result type about half the time. The other 
    ///// times it generate one of the given exceptions.
    //let throwingFunction<'T,'U when 'T:equality> (exceptions:seq<Exception>) (target: Arbitrary<'U>) :Arbitrary<'T->'U> = 
    //    let exc = exceptions |> Seq.toArray
    //    let throwExc = Gen.elements exc |> Gen.map raise
    //    let gen = Gen.frequency [ (exc.Length, target.Generator)
    //                              (exc.Length, throwExc) ]
    //              |> Gen.pureFunction
    //    fromGen gen
    
    //let distinctBy (projection: 'T -> 'Key) (seq: Arbitrary<seq<'T>>) =
    //    seq
    //    |> unArb


    /// Generates Set<'T> values.
    let set (elements: Arbitrary<'T>) = 
        list elements
        |> map Set.ofList

     /// Generates Map<TKey,TValue> values.
     /// Not named `map` because that has other meanings.
    let mapKV (keys: Arbitrary<'TKey>, values: Arbitrary<'TValue>) = 
        // "Natural" implementation:
        // zip keys values
        // |> list
        // |> map Map.ofList
        // however shrinking doesn't work well because it generates 
        // duplicate keys as a result of `list`, which then get 
        // removed by `Map.ofList`. When shrinking, this creates
        // a lot of duplicate shrink attempts, because the list shrinker
        // is removing the elements one by one but this has no effect on
        // the resulting Map.
        let ((Arbitrary keysGen),(Arbitrary valuesGen)) = keys, values
        Gen.map2 (Shrink.map2 (fun x y -> x,y)) keysGen valuesGen
        |> Gen.listOf
        |> Gen.map (List.distinctBy (Shrink.getValue >> fst >> fst))
        |> Gen.map (Shrink.sequenceToList 
                    >> Shrink.map Map.ofList
                    >> Shrink.bindInner (Shrink.ofShrinker Shrink.mapShorten id))
        |> Arbitrary




        
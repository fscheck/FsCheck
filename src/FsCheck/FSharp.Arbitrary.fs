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

    /// Generates only the given constant value.
    /// Does not shrink (because there is only one value).
    let constant (value:'T) =
        value
        |> Shrink.ofValue
        |> Gen.constant
        |> Arbitrary

    let choose (l,h) =
        Gen.choose (l,h)
        // TODO: shrink to l, not 0...
        |> Gen.map (Shrink.ofShrinker Shrink.signedNumber id)
        |> Arbitrary

    let choose64 (l,h) = 
        Gen.choose64 (l,h)
        // TODO: shrink to l, not 0...
        |> Gen.map (Shrink.ofShrinker Shrink.signedNumber id)
        |> Arbitrary

    let sized (sizeFun: int -> Arbitrary<'T>) :Arbitrary<'T> =
        Gen.sized (sizeFun >> unArb)
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

    let bind (f: 'T -> Arbitrary<'U>) (Arbitrary (Gen gen): Arbitrary<'T>) =
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
        |> Arbitrary

    [<Struct>]
    type internal ListAccessWrapper<'T> =
        { Count: int
          Item: int -> 'T
        }
        static member Create(xs:seq<_>) =
            match xs with
            | :? array<_> as arr ->
                { Count=arr.Length; Item=fun i -> arr.[i] }
            | :? IReadOnlyList<_> as list ->
                { Count=list.Count; Item=fun i -> list.[i] }
            | :? IList<_> as list ->
                { Count=list.Count; Item=fun i -> list.[i] }
            | _ ->
                let arr = xs |> Seq.toArray
                { Count=arr.Length; Item=fun i -> arr.[i] }

    let elements (xs : seq<'T>) =
        let elems = ListAccessWrapper<_>.Create xs
        choose (0, elems.Count-1)
        |> map elems.Item

    let oneof (gens:seq<Arbitrary<'T>>) = bind id (elements gens)

    let frequency (dist:seq<int*Arbitrary<'T>>) =
        let xs = ListAccessWrapper<_>.Create dist
        let tot = Seq.sumBy fst dist
        let rec pick i n =
            let k,x = xs.[i]
            if n<=k then x else pick (i+1) (n-k)
        if tot <= 0 then 
            invalidArg "dist" "Frequency was called with a sum of probabilities less than or equal to 0. No elements can be generated."
        else
            bind (pick 0) (choose (1,tot))

    let collectToList (f: 'T -> Arbitrary<'U>) (source:seq<'T>) =
        source
        |> Gen.collectToList (f >> unArb)
        |> Gen.map (Shrink.sequenceToList)
        |> Arbitrary

    let sequenceToList (source:seq<Arbitrary<'T>>) =
        collectToList id source

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
    //let array (elements: Arbitrary<'T>) =
    //    let generator = Gen.arrayOf elements.Generator
    //    fromGenShrink(generator, Shrink.array elements.Shrinker)

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
    
    /// Generates Set<'T> values.
    let set (elements: Arbitrary<'T>) = 
        list elements
        |> map Set.ofList

     /// Generates Map<TKey,TValue> values.
     /// Not named `map` because that has other meanings.
    let mapKV (keys: Arbitrary<'TKey>, values: Arbitrary<'TValue>) = 
        zip keys values
        |> list
        |> map Map.ofList
        
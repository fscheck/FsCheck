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
open FsCheck
open FsCheck.Internals

[<RequireQualifiedAccess>]
module Arb =

    let internal unArb (Arbitrary genShrink) = genShrink

    let internal fromGenTree (genShrink: Gen<Shrink<'T>>) : Arbitrary<'T> =
        Arbitrary genShrink

    let toGen arb =
        arb
        |> unArb
        |> Gen.map (Shrink.getValue >> fst)

    let fromGen (gen: Gen<'T>) : Arbitrary<'T> =
        gen
        |> Gen.map Shrink.ofValue
        |> Arbitrary

    let fromGenShrink (gen: Gen<'T>, shrinker: 'T -> seq<'T>) : Arbitrary<'T> =
        gen
        |> Gen.map (Shrink.ofShrinker shrinker id)
        |> Arbitrary

    let sampleWithSeed seed size nbSamples arb =
        arb
        |> unArb
        |> Gen.sampleWithSeed seed size nbSamples
        |> Array.map (Shrink.getValue >> fst)

    let sampleWithSize size nbSamples arb =
        sampleWithSeed (Random.Create()) size nbSamples arb

    let sample nbSamples arb = sampleWithSize 50 nbSamples arb

    let shrinkSample arb =
        arb
        |> unArb
        |> Gen.sample 1
        |> Array.head
        |> Shrink.getValue
        |> fun (generated, shrinks) ->
            generated,
            seq {
                for shrink in shrinks do
                    yield shrink |> Shrink.getValue |> fst
            }

    let constant value =
        value
        |> Shrink.ofValue
        |> Gen.constant
        |> Arbitrary

    let rec choose (l, h) =
        if l > h then choose (h, l)
        else
            Gen.choose (l, h)
            |> Gen.map (Shrink.ofShrinker (Shrink.signedNumberBetween l h) id)
            |> Arbitrary

    let rec choose64 (l, h) =
        if l > h then choose64 (h, l)
        else
            Gen.choose64 (l, h)
            |> Gen.map (Shrink.ofShrinker (Shrink.signedNumberBetween l h) id)
            |> Arbitrary

    let sized (sizeFun: int -> Arbitrary<'T>) : Arbitrary<'T> =
        Gen.sized (sizeFun >> unArb)
        |> Arbitrary

    let resize size (Arbitrary genShrink: Arbitrary<'T>) : Arbitrary<'T> =
        Gen.resize size genShrink
        |> Arbitrary

    let map f (Arbitrary genShrink) =
        genShrink
        |> Gen.map (Shrink.map f)
        |> Arbitrary

    let map2 f (Arbitrary arb1) (Arbitrary arb2) =
        Gen.map2 (Shrink.map2 f) arb1 arb2
        |> Arbitrary

    let map3 f arb1 arb2 arb3 =
        map2 (fun (a, b) c -> f a b c) (map2 (fun a b -> a, b) arb1 arb2) arb3

    let apply (f: Arbitrary<'T -> 'U>) (arb: Arbitrary<'T>) =
        map2 (fun f a -> f a) f arb

    let private bindGenShrink f (Gen gen) =
        Gen (fun size seed ->
            let struct (source, seed1) = gen size seed
            let value, _ = Shrink.getValue source
            let (Arbitrary (Gen selectedGenerator)) = f value
            let struct (root, seed2) = selectedGenerator size seed1
            let result =
                Shrink.bindRoot root (fun shrunk ->
                    let (Arbitrary (Gen shrinkGenerator)) = f shrunk
                    let struct (tree, _) = shrinkGenerator size seed1
                    tree) source
            struct (result, seed2))

    let bind f (Arbitrary genShrink) =
        bindGenShrink f genShrink
        |> Arbitrary

    let filter predicate (Arbitrary genShrink) =
        genShrink
        |> Gen.filter (Shrink.getValue >> fst >> predicate)
        |> Gen.map (Shrink.filter predicate)
        |> Arbitrary

    let mapFilter mapper predicate arb =
        arb
        |> map mapper
        |> filter predicate

    let convert convertTo _convertFrom arb = map convertTo arb

    let elements (xs: seq<'T>) =
        let elems = ListAccessWrapper<_>.Create xs
        choose (0, elems.Count - 1)
        |> map elems.Item

    let frequency (distribution: seq<int * Arbitrary<'T>>) =
        let entries = ListAccessWrapper<_>.Create distribution
        let mutable total = 0
        let starts =
            Array.init entries.Count (fun index ->
                let weight, _ = entries.Item index
                if weight < 0 then
                    invalidArg "distribution" "Frequency weights must be non-negative."
                let start = total
                total <- total + weight
                if weight = 0 then -1 else start)

        if total <= 0 then
            invalidArg "distribution" "Frequency requires a positive sum of weights."

        let rec pick index value =
            let weight, arb = entries.Item index
            if value < weight then arb
            else pick (index + 1) (value - weight)

        let shrinkTicket ticket =
            let selected =
                Array.findIndex (fun index ->
                    starts.[index] >= 0
                    && ticket >= starts.[index]
                    && ticket < starts.[index] + fst (entries.Item index)) [| 0 .. entries.Count - 1 |]
            seq {
                for index in 0 .. selected - 1 do
                    if starts.[index] >= 0 then
                        yield starts.[index]
            }

        fromGenShrink (Gen.choose (0, total - 1), shrinkTicket)
        |> bind (fun ticket -> pick 0 ticket)

    let oneof (arbs: seq<Arbitrary<'T>>) =
        arbs
        |> Seq.map (fun arb -> 1, arb)
        |> frequency

    let collectToList (f: 'T -> Arbitrary<'U>) (source: seq<'T>) =
        source
        |> Gen.collectToList (f >> unArb)
        |> Gen.map Shrink.sequenceToList
        |> Arbitrary

    let sequenceToList (source: seq<Arbitrary<'T>>) =
        collectToList id source

    let collectToArray (f: 'T -> Arbitrary<'U>) (source: seq<'T>) =
        source
        |> Gen.collectToArray (f >> unArb)
        |> Gen.map Shrink.sequenceToArray
        |> Arbitrary

    let sequenceToArray (source: seq<Arbitrary<'T>>) =
        collectToArray id source

    let bool = elements [ false; true ]

    let double maxAbsValue =
        Gen.map2
            (fun fraction negative ->
                let value = fraction * float maxAbsValue
                if negative then -value else value)
            Gen.double
            Gen.bool
        |> Gen.map (Shrink.ofShrinker Shrink.double id)
        |> Arbitrary

    let option value =
        frequency [ 1, constant None; 7, map Some value ]

    let nullable (value: Arbitrary<'T>) =
        frequency [ 1, constant (Nullable()); 7, map Nullable value ]

    let list (elements: Arbitrary<'T>) =
        elements
        |> unArb
        |> Gen.listOf
        |> Gen.map (Shrink.sequenceToList >> Shrink.bindInner (Shrink.ofShrinker Shrink.listShorten id))
        |> Arbitrary

    let array (elements: Arbitrary<'T>) =
        elements
        |> unArb
        |> Gen.arrayOf
        |> Gen.map (Shrink.sequenceToArray >> Shrink.bindInner (Shrink.ofShrinker Shrink.arrayShorten id))
        |> Arbitrary

    let pureFunction (target: Arbitrary<'U>) : Arbitrary<'T -> 'U> =
        target
        |> toGen
        |> Gen.pureFunction
        |> fromGen

    let throwingFunction<'T, 'U when 'T: equality>
        (exceptions: seq<Exception>)
        (target: Arbitrary<'U>)
        : Arbitrary<'T -> 'U> =
        let exceptions = Seq.toArray exceptions
        let throwException = Gen.elements exceptions |> Gen.map raise
        Gen.frequency
            [ exceptions.Length, target |> toGen
              exceptions.Length, throwException ]
        |> Gen.pureFunction
        |> fromGen

    let set (elements: Arbitrary<'T>) =
        elements
        |> list
        |> map Set.ofList

    let mapKV (keys: Arbitrary<'TKey>, values: Arbitrary<'TValue>) =
        let (Arbitrary keyGen), (Arbitrary valueGen) = keys, values
        Gen.map2 (Shrink.map2 (fun key value -> key, value)) keyGen valueGen
        |> Gen.listOf
        |> Gen.map (List.distinctBy (Shrink.getValue >> fst >> fst))
        |> Gen.map (
            Shrink.sequenceToList
            >> Shrink.map Map.ofList
            >> Shrink.bindInner (Shrink.ofShrinker Shrink.mapShorten id))
        |> Arbitrary

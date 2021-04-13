namespace FsCheck.Internals

open System
open System.Collections.Generic

/// Shrink<T> keeps a value and the possible shrinks for that value in an n-ary tree.
/// This tree is explored by the test runner to find a smaller value of a counter-example.
[<NoEquality; NoComparison>]
type Shrink<'T> = private ShrinkTree of Lazy<'T> * seq<Shrink<'T>>

module internal Shrink =


    /// Evaluate the top value in the shrink tree, and return it, as well
    /// as the rest of the Shrinks
    let getValue (ShrinkTree (Lazy v,t)) = (v, t)

    let sample shrink =
        let success = ResizeArray<'T>()
        let mutable next = shrink |> getValue |> snd |> Seq.tryHead
        while next.IsSome do
            success.Add(next.Value |> getValue |> fst)
            next <- next.Value |> getValue |> snd |> Seq.tryHead

        let fail = shrink |> getValue |> snd |> Seq.map (getValue >> fst) |> Seq.toArray
        {| Original = shrink |> getValue |> fst
           Success = success.ToArray()
           Fail = fail
        |}

    /// Creates a Shrink<'T> for the given value with no shrinks.
    let ofValue x = ShrinkTree(lazy (x), Seq.empty)

    /// Creates a Shrink<'T> for the given lazy value with no shrinks.
    let ofLazy x = ShrinkTree(x, Seq.empty)

    /// Creates a Shrink<T> from the given shrink function. Also
    /// applies the given function f to each shrink (in one function for
    /// performance reasons, otherwise equivalent to ofShrinker shrink x |> map f).
    let ofShrinker shrink (f:'T -> 'U) x =
        //cache is important here to avoid re-evaluation of property
        let rec buildTree x = ShrinkTree (lazy (f x), shrink x |> Seq.map buildTree |> Seq.cache)
        buildTree x

    let rec map (f: 'T -> 'U) (ShrinkTree (x, rs)) =
        ShrinkTree(lazy (f x.Value), rs |> Seq.map (map f))

    let rec map2 (f: 'T1 -> 'T2 -> 'U) r1 r2 =
        let (ShrinkTree (v1, s1)) = r1
        let (ShrinkTree (v2, s2)) = r2
        ShrinkTree(
            lazy (f v1.Value v2.Value),
            seq {
                for left in s1 do
                    yield! s2 |> Seq.map (map2 f left)
                for right in s2 do
                    yield! s1 |> Seq.map (fun l -> map2 f l right) })

    let rec filter (f: 'T -> bool) (ShrinkTree (x,rs)) =
        ShrinkTree (x, 
            seq {
                for (ShrinkTree (y,ts)) in rs do
                    if f y.Value then
                        yield ShrinkTree(y, ts |> Seq.map (filter f)) })

    ///// Returns a new tree which never tries any value from the underlying
    ///// tree twice, based on the key function.
    //let distinctBy (f: 'T -> 'Key) (ShrinkTree (x,_) as source) :Shrink<'T> =
    //    let keys = HashSet<'Key>()
    //    let rec distinctByHelper (ShrinkTree (x,rs)) =
    //        ShrinkTree(x,
    //            seq {
    //                for (ShrinkTree (y,ts)) in rs do
    //                    if (keys.Add(f y.Value)) then
    //                        yield ShrinkTree(y, ts |> Seq.map distinctByHelper)
    //            }
    //        )
    //    keys.Add(f x.Value) |> ignore
    //    distinctByHelper source


    let rec join (ShrinkTree (r, tts)): Shrink<'T> =
        // Note 1: we can't pattern match as follows, as this will result in evaluation:
        //         let rec join (ShrinkTree (Lazy (ShrinkTree(x,ts)),tts))
        //         instead, define everything inside the ShrinkTree ctor, as a lazily evaluated expression.
        // Note 2: This first shrinks outer quantification, this makes the most sense.
        //         shrink inner quantification can done like: ShrinkTree (x,(Seq.append ts (Seq.map join tts)))
        //let x =
        //    lazy (let (ShrinkTree (x, _)) = r.Value in x.Value)
        let rval,rtree = r.Value |> getValue
        let ts =
            Seq.append
                (Seq.map join tts)
                rtree //(match r with (Lazy (ShrinkTree (_, ts))) -> ts)
        ShrinkTree(lazy rval, ts)

    let bind (k: 'T -> Shrink<'U>) m = map k m |> join

    /// Same as bind, but shrinks inner bind first.
    let internal bindInner (k: 'T -> Shrink<'U>) m =
        let rec joinInner (ShrinkTree (r,tts)) =
            let x =
                lazy (let (ShrinkTree (x, _)) = r.Value in x.Value)
            let ts =
                Seq.append
                    (match r with (Lazy (ShrinkTree (_, ts))) -> ts)
                    (Seq.map join tts)
            ShrinkTree(x, ts)

        map k m |> joinInner

    let collectToList (f:'T->Shrink<'U>) (source: seq<'T>) :Shrink<list<'U>> =
        let s = source |> Seq.map f |> Seq.toList
        let rec shrinkTreeList (l:list<Shrink<'U>>) =
            match l with
            | [] -> Seq.empty
            | (x::xs) -> 
                seq {
                    let rest = xs |> List.map (getValue >> fst)
                    let (v,vs) = x |> getValue
                    for t in vs do
                        yield t |> map (fun t -> t::rest)
                    for xs' in shrinkTreeList xs do
                        yield xs' |> map (fun xs' -> v::xs') }
        ShrinkTree(lazy (s |> List.map (getValue >> fst)), shrinkTreeList s)

    let sequenceToList (source:seq<Shrink<'T>>) =
        collectToList id source

    let rec collectToArray (f:'T->Shrink<'U>) (source: seq<'T>) :Shrink<'U[]> =
        let s = source |> Seq.map f |> Seq.toArray
        // the array as it was, i.e. no shrinking.
        // this will be copied every time we shrink one of the 
        // elements, one by one.
        let value = s |> Array.map (getValue >> fst)
        let rec shrinkTreeArray value (shrinkers:Shrink<'U>[]) =
            seq {
                // Shrink one element at a time, first to last,
                // exhausting each of the element shrinkers
                let mutable i = 0
                for elemShrinker in shrinkers do
                    let (_,vs) = elemShrinker |> getValue
                    for t in vs do
                        yield t |> map (fun t -> 
                            let copy = Array.copy value
                            copy.[i] <- t
                            copy
                        )
                    i <- i + 1
            }
        ShrinkTree(lazy value, shrinkTreeArray value s)

    let sequenceToArray (source:seq<Shrink<'T>>) =
        collectToArray id source

    // The sequence n-n/2^i for i starting at 1.
    // e.g. for 128: 64, 96, 112,...
    // In other words bisects the range 0..n successively, and 
    // always chooses higher number.
    let inline private bisectIncreasing n =
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        seq { let mutable st = n / two
              while st <> LanguagePrimitives.GenericZero do
                  yield n-st
                  st <- st / two
        }

    let inline private invert n = 
        if n = Numeric.MinValue.Get() then
            Numeric.MaxValue.Get()
        else
            -n

    /// A generic shrinker for signed numbers.
    let inline signedNumber n =
        seq { if n <> LanguagePrimitives.GenericZero then yield LanguagePrimitives.GenericZero
              if n < LanguagePrimitives.GenericZero then yield invert n
              yield! bisectIncreasing n }

    let inline signedNumberBetween lo hi n =
        signedNumber n
        |> Seq.where (fun v -> lo <= v && v <= hi)

    /// A generic shrinker for unsigned numbers.
    let inline unsignedNumber n =
        seq { if n <> LanguagePrimitives.GenericZero then yield LanguagePrimitives.GenericZero
              yield! bisectIncreasing n }

    let inline double n =
        let (|<|) x y = abs x < abs y
        seq { if n <> 0.0 then yield 0.0
              if n < 0.0 then yield -n
              let truncated = truncate n
              if truncated |<| n then yield truncated }
        |> Seq.distinct

    let internal date (d:DateTime) =
        if d.Kind <> DateTimeKind.Unspecified then
            seq { yield DateTime.SpecifyKind(d, DateTimeKind.Unspecified) }
        elif d.Millisecond <> 0 then
            seq { yield DateTime(d.Year,d.Month,d.Day,d.Hour,d.Minute,d.Second) }
        elif d.Second <> 0 then
            seq { yield DateTime(d.Year,d.Month,d.Day,d.Hour,d.Minute,0) }
        elif d.Minute <> 0 then
            seq { yield DateTime(d.Year,d.Month,d.Day,d.Hour,0,0) }
        elif d.Hour <> 0 then
            seq { yield DateTime(d.Year,d.Month,d.Day) }
        else
            Seq.empty

    /// Shrink a list by taking a single element away in each shrink attempt.
    let internal listShorten l =
        let rec shrinkList l =
            match l with
            | [] ->      Seq.empty
            | (x::xs) -> seq { yield xs
                               for xs' in shrinkList xs -> x::xs' }
        shrinkList l

    /// Shrink a list by shrinking each element using the given elementShrink.
    /// The shrunk lists all have the same length as the original list.
    let internal listElements (elementShrink: 'T -> #seq<'T>) l = 
        let rec shrinkList l =
            match l with
            | [] ->      Seq.empty
            | (x::xs) -> seq { for x' in elementShrink x -> x'::xs
                               for xs' in shrinkList xs -> x::xs'
                            }
        shrinkList l

    /// Shrink a list by combining listLength and listElements.
    let internal list (elementShrink: 'T -> #seq<'T>) l =
        let length = listShorten l
        let elements = listElements elementShrink l
        Seq.append length elements

    let arrayShorten (a: 'T[]) =
        let prepend x (arr : _[]) =
            let len = arr.Length
            if len = 0 then [| x |]
            else
                let result = Array.zeroCreate (len + 1)
                result.[0] <- x
                Array.blit arr 0 result 1 len
                result

        let rec shrinkArray (arr : 'T[]) =
            if Array.isEmpty arr then Seq.empty else
            seq {
                let x = arr.[0]
                let xs = arr.[1..]
                yield xs
                for xs' in shrinkArray xs -> prepend x xs'
            }
        shrinkArray a

    let arrayElements elementShrink arr =
        let rec shrinkArray (arr : 'T[]) =
            if Array.isEmpty arr then Seq.empty else
            seq {
                for i in 0..arr.Length-1 do
                    for x' in elementShrink arr.[i] do
                        let copy = Array.copy arr
                        copy.[i] <- x'
                        yield copy
            }
        shrinkArray arr

    let array elementShrink arr =
        let length = arrayShorten arr
        let elements = arrayElements elementShrink arr
        Seq.append length elements

    let mapShorten (map: Map<'K,'V>) =
        seq { for (key,_) in map |> Map.toSeq do
                yield Map.remove key map
        }



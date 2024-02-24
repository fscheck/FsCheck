namespace FsCheck.Internals

open System

/// Shrink<T> keeps a value and the possible shrinks for that value in an n-ary tree.
/// This tree is explored by the test runner to find a smaller value of a counter-example.
[<NoEquality; NoComparison>]
type Shrink<'T> = private ShrinkTree of Lazy<'T> * seq<Shrink<'T>>

module internal Shrink =

    /// Creates a Shrink<'T> for the given value with no shrinks.
    let ofValue x = ShrinkTree(lazy (x), Seq.empty)

    /// Creates a Shrink<'T> for the given lazy value with no shrinks.
    let ofLazy x = ShrinkTree(x, Seq.empty)

    /// Creates a Shrink<T> from the given shrink function. Also
    /// applies the given function f to each shrink (in one function for
    /// performance reasons, otherwise equivalent to ofShrinker shrink x |> map f).
    let ofShrinker shrink x (f:'T -> 'U) =
        //cache is important here to avoid re-evaluation of property
        let rec buildTree x = ShrinkTree (lazy (f x), shrink x |> Seq.map buildTree |> Seq.cache)
        buildTree x

    let rec map (f: 'T -> 'U) (ShrinkTree (x, rs)) =
        ShrinkTree(lazy (f x.Value), rs |> Seq.map (map f))

    let rec map2 (f: 'T1 -> 'T2 -> 'U) r1 r2 =
        let (ShrinkTree (v1, s1)) = r1
        let (ShrinkTree (v2, s2)) = r2
        ShrinkTree(lazy (f v1.Value v2.Value), Seq.map2 (map2 f) s1 s2)

    let rec join (ShrinkTree (r, tts)): Shrink<'T> =
        // Note 1: we can't pattern match as follows, as this will result in evaluation:
        //         let rec join (ShrinkTree (Lazy (ShrinkTree(x,ts)),tts))
        //         instead, define everything inside the ShrinkTree ctor, as a lazily evaluated expression.
        // Note 2: This first shrinks outer quantification, this makes the most sense.
        //         shrink inner quantification can done like: ShrinkTree (x,(Seq.append ts (Seq.map join tts)))
        let x =
            lazy (let (ShrinkTree (x, _)) = r.Value in x.Value)

        let ts =
            Seq.append
                (Seq.map join tts)
                (match r with
                 | (Lazy (ShrinkTree (_, ts))) -> ts)

        ShrinkTree(x, ts)

    let bind m (k: 'T -> Shrink<'U>) = map k m |> join

    /// Evaluate the top value in the shrink tree, and return it, as well
    /// as the rest of the Shrinks
    let getValue (ShrinkTree (Lazy v,t)) = (v, t)

    // The sequence n-n/2^i for i starting at 1.
    // e.g. for 128: 64, 96, 112,...
    // In other words bisects the range 0..n successively, and 
    // always chooses higher number.
    let inline private bisecIncreasing n =
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        seq { let mutable st = n / two
              while st <> LanguagePrimitives.GenericZero do
                yield n-st
                st <- st / two
                
        }
        //Seq.unfold (fun st -> let st = st / two in Some (n-st, st)) n 

    /// A generic shrinker for signed numbers.
    let inline signedNumber n =
        let inline invert n = 
            if n = Numeric.MinValue.Get() then
                Numeric.MaxValue.Get()
            else
                -n
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        seq { if n < LanguagePrimitives.GenericZero then yield invert n
              if n <> LanguagePrimitives.GenericZero then yield LanguagePrimitives.GenericZero
              yield! bisecIncreasing n }

    /// A generic shrinker for unsigned numbers.
    let inline unsignedNumber n =
        seq { if n <> LanguagePrimitives.GenericZero then yield LanguagePrimitives.GenericZero
              yield! bisecIncreasing n }

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

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
    let run (ShrinkTree (Lazy v,t)) = (v, t)

    ///A generic shrinker that should work for most number-like types.
    let inline number n =
        let (|>|) x y = abs x > abs y 
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        seq {   if n < LanguagePrimitives.GenericZero then yield -n
                if n <> LanguagePrimitives.GenericZero then yield LanguagePrimitives.GenericZero
                yield! Seq.unfold (fun st -> let st = st / two in Some (n-st, st)) n 
                        |> Seq.takeWhile ((|>|) n) }
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

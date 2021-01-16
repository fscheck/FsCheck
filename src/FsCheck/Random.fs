namespace FsCheck

(*
The implementation in this file is the implementation of SplitMix algorithm given in:

Fast Splittable Pseudorandom Number Generators, Guy L. Steele Jr., Doug Lea and Christine H. Flood.

It is also incorporated as such in JDK 8 in the class SplittableRandom.java.

This algorithm has been tested by the authors and found to have reasonable statistical properties
(as opposed to the algorithm used by FsCheck 2.x, which was taken from Haskell and by its own admission
has "no statistical basis") and is also pretty fast.

The main difference with the paper is that I've chosen here to make a functional API, i.e. where
all the functions are pure and the state is carried explicitly using the type Rnd. 
This approach fits better with how FsCheck Generators work.
*)

open System.Runtime.InteropServices

module private RandomConstants =

    let [<Literal>] GOLDEN_GAMMA = 0x9e3779b97f4a7c15UL // must be an odd number

    //the positive difference between 1.0 and the smallest double value > 1.0
    let [<Literal>] DOUBLE_MULTIPLIER = 1.1102230246251565e-016 // 1.0 / double (1L <<< 53)

[<Struct>]
type Rnd =
    /// Seed value for the random number generator.
    val Seed: uint64
    /// An odd integer
    val Gamma: uint64

    /// Create a new random number generator with the given seed and gamma.
    /// Useful to faithfully reproduce a sequence or part of it. gamma must be odd,
    /// or this throws invalid argument exception. For good pseudo-random properties,
    /// please only use seeds and gamma that were generated as part of a sequence
    /// started with the default create function.
    new(seed, gamma) =
        if (gamma % 2UL = 0UL) then
            invalidArg "gamma" (sprintf "Gamma must be odd, given: %i" gamma)

        { Seed = seed; Gamma = gamma }

    /// Create a new Rnd value with the specified seed value and the 'golden' gamma.
    new(seed) = { Seed = seed; Gamma = RandomConstants.GOLDEN_GAMMA }

    member internal this.Next () =
        Rnd (this.Seed + this.Gamma, this.Gamma)

    static member internal Mix64 z =
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdUL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53UL
        z ^^^ (z >>> 33)

    static member internal Mix32Variant04 z =
        let z = (z ^^^ (z >>> 33)) * 0x62a9d9ed799705f5UL
        int (((z ^^^ (z >>> 28)) * 0xcb24d0a5c88c35b3UL) >>> 32)

    static member internal Mix64vVriant13 z =
        let z = (z ^^^ (z >>> 30)) * 0xbf58476d1ce4e5b9UL
        let z = (z ^^^ (z >>> 27)) * 0x94d049bb133111ebUL
        z ^^^ (z >>> 31)

    static member internal BitCount i =
        let i = i - ((i >>> 1) &&& 0x5555555555555555UL)
        let i = (i &&& 0x3333333333333333UL) + ((i >>> 2) &&& 0x3333333333333333UL)
        (((i + (i >>> 4)) &&& 0xF0F0F0F0F0F0F0FUL) * 0x101010101010101UL) >>> 56

    static member internal MixGamma z =
        let z = (Rnd.Mix64 z) ||| 1UL //make odd
        let n = Rnd.BitCount (z ^^^ (z >>> 1))
        //Fig 16 in the paper contains a bug here - it does n >= 24.
        if (n < 24UL) then z ^^^ 0xaaaaaaaaaaaaaaaaUL else z //This result is always odd.

    override t.ToString() = sprintf "%i,%i" t.Seed t.Gamma


[<AbstractClass; Sealed>]
type internal Random () =
    static let defaultGen = System.DateTime.UtcNow.Ticks |> uint64 |> Rnd.Mix64vVriant13 |> ref

    static member DefaultGen = defaultGen

    ///Create a new random number generator with the given seed and a "golden" gamma.
    static member CreateWithSeed seed =
        Rnd(seed)

    ///Create a new random number generator with the given seed and gamma. Useful to faithfully reproduce a sequence
    ///or part of it. gamma must be odd, or this throws invalid argument exception. For good pseudo-random properties,
    ///please only use seeds and gamma that were generated as part of a sequence started with the default create function.
    static member CreateWithSeedAndGamma (seed, gamma) =
        Rnd(seed, gamma)

    ///Create a new random number generator that goes through some effort to generate a new seed
    ///every time it's called, and also to generate different seeds on different machines, though
    ///the latter is not at all guaranteed.
    static member Create() =
        let s = lock defaultGen <| fun () ->
            let r = !defaultGen
            defaultGen := r + 2UL * RandomConstants.GOLDEN_GAMMA
            r

        Rnd(Rnd.Mix64vVriant13 s, Rnd.MixGamma(s + RandomConstants.GOLDEN_GAMMA))

    ///Generate the next pseudo-random uint64 in the sequence, and return the new Rnd.
    static member private NextUInt64 ([<Out>] s : byref<Rnd>) =
        s <- s.Next()
        Rnd.Mix64vVriant13 s.Seed

    ///Generate the next pseudo-random int64 in the sequence, and return the new Rnd.
    static member NextInt64 ([<Out>] s : byref<Rnd>) =
        s <- s.Next()
        int64 <| Rnd.Mix64vVriant13 s.Seed

    ///Generate the next pseudo-random int in the sequence, and return the new Rnd.
    static member NextInt ([<Out>] s : byref<Rnd>) =
        s <- s.Next()
        Rnd.Mix32Variant04 s.Seed

    ///Generate the next pseudo-random float in the sequence in the interval [0, 1] and return the new Rnd.
    static member NextFloat ([<Out>] s : byref<Rnd>) =
        let l = Random.NextUInt64 (&s)
        double (l >>> 11) * RandomConstants.DOUBLE_MULTIPLIER

    ///Split this PRNG in two PRNGs that overlap with very small probability.
    static member Split (state : Rnd, [<Out>] left : byref<Rnd>, [<Out>] right : byref<Rnd>) : unit =
        let s1 = state.Next ()
        let s2 = s1.Next ()
        left <- s2
        right <- Rnd(Rnd.Mix64vVriant13 s1.Seed, Rnd.MixGamma s2.Seed)

    ///Generate the next pseudo-random int64 in the given range (inclusive l and h) and return the new Rnd.
    static member RangeInt64 (l, h, s : Rnd, [<Out>] s' : byref<Rnd>) =
        let mutable s'' = s
        if l > h then //swap l and h
            Random.RangeInt64 (h, l, s, &s')
        else
            let m = h - l  //size of the exclusive range
            let n = m + 1L //size of the inclusive range
            let mutable candidate = Random.NextInt64 (&s'')
            let mutable result = System.Nullable<_> ()
            if (n > 0L) then
                //we have a range that is a positive int64, i.e. a "small" range relative to the total range - half of it or less.
                //So we need to be a bit smart on how to find an int64 in that range.
                while not result.HasValue do
                    //since the range is half or less, we create a positive int64 by shifting right.
                    let pcand = int64 (uint64 candidate >>> 1)
                    //a good candidate offset from l, i.e. offset < n.
                    let offset = pcand % n
                    //
                    if pcand + m - offset < 0L then
                        candidate <- Random.NextInt64 (&s'')
                    else
                        result <- System.Nullable<_> (l + offset)
            else 
                //we have a range that is a negative int64, i.e. h -l overflowed and so we have "large" range - more than half the total range.
                //we can just keep generating until we find a number in the desired range.
                while not result.HasValue do
                    if candidate < l || candidate >= h then
                        candidate <- Random.NextInt64 (&s'')
                    else
                        result <- System.Nullable<_> (candidate)
            
            // Return the result
            s' <- s''
            result.Value

    ///Generate the next pseudo-random int in the given range (inclusive l and h) and returns the new Rnd.
    static member RangeInt (l, h, s : Rnd, [<Out>] s' : byref<Rnd>) : int =
        let mutable s'' = s
        if l > h then
            Random.RangeInt (h, l, s, &s')
        else
            let m = h - l
            let n = m + 1
            let mutable candidate = Random.NextInt (&s'')
            let mutable result = System.Nullable<_> ()
            if (n > 0) then
                while not result.HasValue do
                    let pcand = int (uint32 candidate >>> 1)
                    let offset = pcand % n
                    if pcand + m - offset < 0 then
                        candidate <- Random.NextInt (&s'')
                    else
                        result <- System.Nullable<_> (l + offset)
            else 
                while not result.HasValue do
                    if candidate < l || candidate >= h then 
                        candidate <- Random.NextInt (&s'')
                    else
                        result <- System.Nullable<_> (candidate)

            // Return the result.
            s' <- s''
            result.Value

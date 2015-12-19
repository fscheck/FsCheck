(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                                      **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

(*
The implementation in this file is the implementation of SplitMix algorithm given in:

Fast Splittable Pseudorandom Number Generators, Guy L. Steele Jr., Doug Lea and Christine H. Flood.

It is also incorporated as such in JDK 8 in the class SplittableRandom.java.

This algorithm has been tested by the authors and found to have reasonable statistical properties
(as opposed to the algortihm used by FsCheck before, which was taken from Haskell and by its own admission
has "no statistical basis") and is also pretty fast.

The main difference with the paper is that I've chosen here to make an functional API, i.e. where
all the functions are pure and the state is carried explicitly using the type Rnd. 
This approach fits better with how FsCheck Generators work.
*)

module Random =

    let [<Literal>] private GOLDEN_GAMMA = 0x9e3779b97f4a7c15UL // must be an odd number

    //the positive difference between 1.0 and the smallest double value > 1.0
    let [<Literal>] private DOUBLE_MULTIPLIER = 1.1102230246251565e-016 // 1.0 / double (1L <<< 53)
  
    [<Struct>]
    type Rnd = 
        val Seed: uint64
        val Gamma: uint64 //an odd integer
        new(seed, gamma) = { Seed = seed; Gamma = gamma }
        override t.ToString() = sprintf "%i,%i" t.Seed t.Gamma
                         
    let private next (state:Rnd) = 
        Rnd(state.Seed + state.Gamma, state.Gamma)
    
    let private mix64 z =
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdUL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53UL
        z ^^^ (z >>> 33)

    let private mix32variant04 z =
        let z = (z ^^^ (z >>> 33)) * 0x62a9d9ed799705f5UL
        int (((z ^^^ (z >>> 28)) * 0xcb24d0a5c88c35b3UL) >>> 32)

    let private mix64variant13 z =
        let z = (z ^^^ (z >>> 30)) * 0xbf58476d1ce4e5b9UL
        let z = (z ^^^ (z >>> 27)) * 0x94d049bb133111ebUL
        z ^^^ (z >>> 31)

    let private bitCount i =
        let i = i - ((i >>> 1) &&& 0x5555555555555555UL)
        let i = (i &&& 0x3333333333333333UL) + ((i >>> 2) &&& 0x3333333333333333UL)
        (((i + (i >>> 4)) &&& 0xF0F0F0F0F0F0F0FUL) * 0x101010101010101UL) >>> 56

    let private mixGamma z =
        let z = (mix64 z) ||| 1UL //make odd
        let n = bitCount (z ^^^ (z >>> 1))
        //Fig 16 in the paper contains a bug here - it does n >= 24.
        if (n < 24UL) then z ^^^ 0xaaaaaaaaaaaaaaaaUL else z //This result is always odd.

    let private defaultGen = System.DateTime.Now.Ticks |> uint64 |> mix64variant13 |> ref

    ///Create a new random number generator with the given seed and a "golden" gamma.
    let createWithSeed seed =
        Rnd(seed, GOLDEN_GAMMA)

    ///Create a new random number generator with the given seed and gamma. Useful to faithfully reproduce a sequence
    ///or part of it. gamma must be odd, or this throws invalid argument exception. For good pseudo-random properties,
    ///please only use seeds and gamma that were generated as part of a sequence started with the default create function.
    let createWithSeedAndGamma (seed, gamma) =
        if (gamma % 2UL = 0UL) then invalidArg "gamma" "Gamma must be odd."
        Rnd(seed, gamma)

    ///Create a new random number generator that goes through some effort to generate a new seed
    ///every time it's called, and also to generate different seeds on different machines, though
    ///the latter is not at all guaranteed.
    let create() =
        let s = lock defaultGen (fun () -> let r = !defaultGen in defaultGen := r + 2UL * GOLDEN_GAMMA; r)
        Rnd(mix64variant13 s, mixGamma(s + GOLDEN_GAMMA))

    let private nextUInt64 s =
        let s' = next s
        mix64variant13 s'.Seed, s'

    ///Generate the next pseudo-random int64 in the sequence, and return the new Rnd.
    let nextInt64 s =
        let s' = next s
        int64 <| mix64variant13 s'.Seed, s'

    ///Generate the next pseudo-random int in the sequence, and return the new Rnd.
    let nextInt s =
        let s' = next s
        mix32variant04 s'.Seed, s'

    ///Generate the next pseudo-random float in the sequence in the interval [0, 1[ and return the new Rnd.
    let nextFloat s =
        let l,s' = nextUInt64 s
        double (l >>> 11) * DOUBLE_MULTIPLIER, s'

    ///Split this PRNG in two PRNGs that overlap with very small probability.
    let split s0 =
        let s1 = next s0
        let s2 = next s1
        s2, Rnd(mix64variant13 s1.Seed, mixGamma s2.Seed)

    ///Generate the next pseudo-random int64 in the given range (inclusive l and h) and return the new Rnd.
    let rec rangeInt64 (l,h) s =
        if l > h then //swap l and h
            rangeInt64 (h,l) s
        else
            let maybeResult = nextInt64 s
            let m = h - l  //size of the exclusive range
            let n = m + 1L //size of the inclusive range
            if (n > 0L) then
                //we have a range that is a positive int64, i.e. a "small" range relative to the total range - half of it or less.
                //So we need to be a bit smart on how to find an int64 in that range.
                let rec iter (cand,s) =
                    //since the range is half or less, we create a positive int64 by shifting right.
                    let pcand = int64 (uint64 cand >>> 1)
                    //a good candidate offset from l, i.e. offset < n.
                    let offset = pcand % n
                    //
                    if pcand + m - offset < 0L then
                        iter (nextInt64 s)
                    else
                        l + offset, s
                iter maybeResult
            else 
                //we have a range that is a negative int64, i.e. h -l overflowed and so we have "large" range - more than half the total range.
                //we can just keep generating until we find a number in the desired range.
                let rec iter (cand, s) =
                    if cand < l || cand >= h then 
                        iter (nextInt64 s)
                    else
                        cand, s
                iter maybeResult

    ///Generate the next pseudo-random int in the given range (inclusive l and h) and returns the new Rnd.
    let rec rangeInt (l,h) s =
        if l > h then
            rangeInt (h,l) s
        else
            let maybeResult = nextInt s
            let m = h - l
            let n = m + 1
            if (n > 0) then
                let rec iter (cand,s) =
                    let pcand = int (uint32 cand >>> 1)
                    let offset = pcand % n
                    if pcand + m - offset < 0 then
                        iter (nextInt s)
                    else
                        l + offset, s
                iter maybeResult
            else 
                let rec iter (cand, s) =
                    if cand < l || cand >= h then 
                        iter (nextInt s)
                    else
                        cand, s
                iter maybeResult

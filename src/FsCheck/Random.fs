(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2013 Kurt Schelfthout. All rights reserved.          **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

///Generate random numbers based on splitting seeds. Based Hugs' Random implementation.
module Random =

    open System

    type StdGen = StdGen of int * int

    //Haskell has mod,quot, en divMod. .NET has DivRem, % and /.
    // Haskell              | F#
    //---------------------------------
    // rem                  | %
    // mod                  | ?
    // quot                 | /
    // div                  | ?
    // divMod               | ?
    // quotRem              | divRem

    //since the implementation uses divMod and mod, we need to reimplement these.
    //fortunately that's fairly easy given Math.DivRem
    let divMod (n:int) d = 
        let (q,r) as qr = Math.DivRem(n,d) //neat F# feature here: out parameters are converted to tuples!
        if (Math.Sign(r) = -Math.Sign(d)) then (q-1,r+d) else (q,r)

    let divMod64 (n:int64) d = 
        let (q,r) as qr = Math.DivRem(n,d)
        if (Math.Sign(r) = -Math.Sign(d)) then (q-1L,r+d) else (q,r)

    let hMod n d = 
        let _,r = divMod n d
        r

    let hMod64 n d = 
        let _,r = divMod64 n d
        r

    let q1,q2 = 53668,52774
    let a1,a2 = 40014,40692
    let r1,r2 = 12211,3791
    let m1,m2 = 2147483563,2147483399

    let mkStdGen s = 
        let s = if s < 0L then -s else s
        let (q, s1) = divMod64 s (int64 (m1-1))  //2147483562L
        let s2 = hMod64 q (int64 (m2-1)) //2147483398L
        StdGen (int (s1+1L),int (s2+1L))

    let stdNext (StdGen (s1,s2)) =
        let k = s1 / q1
        let s1' = a1 * (s1 - k * q1) - k * r1
        let s1'' = if (s1' < 0) then s1 + m1 else s1'
        let k' = s2 / q2
        let s2' = a2 * (s2 - k' * q2) - k' * r2
        let s2'' = if s2' < 0 then s2' + m2 else s2'
        let z = s1'' - s2''
        let z' = if z < 1 then z + m1 - 1 else z
        (z', StdGen (s1'', s2''))
        
    let stdSplit ((StdGen (s1,s2)) as std) = 
        let new_s1 = if s1 = (m1-1) then 1 else s1 + 1
        let new_s2 = if s2 = 1 then (m2-1) else s2 - 1
        let (StdGen (t1,t2)) = snd (stdNext std) 
        let left = StdGen (new_s1, t2)
        let right = StdGen (t1, new_s2)
        (left,right)

    let rec private iLogBase b i = if i < b then 1 else 1 + iLogBase b (i / b)

    let rec stdRange (l,h) rng =
        if l > h then stdRange (h,l) rng
        else
            let k = h - l + 1
            let b = 2147483561
            let n = iLogBase b k
            let rec f n acc g = 
                if n = 0 then (acc,g) 
                else let (x,g') = stdNext g in f (n-1) (x + acc * b) g'
            match (f n 1 rng) with (v, rng') -> (l + abs( v % k ), rng')

    let range = stdRange
    let split = stdSplit
    let newSeed() = DateTime.Now.Ticks |> mkStdGen
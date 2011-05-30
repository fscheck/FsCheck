namespace FsCheck.Test

module Helpers = 

    open System
    open FsCheck

    let sample n gn  = 
        let rec sample i seed samples =
            if i = 0 then samples
            else sample (i-1) (Random.stdSplit seed |> snd) (Gen.eval 1000 seed gn :: samples)
        sample n (Random.newSeed()) []

    let sample1 gn = sample 1 gn |> List.head
    
    let isIn l elem = List.exists ((=) elem) l

module Random =

    open FsCheck
    open FsCheck.Prop
    open FsCheck.Random
    open Helpers

    [<Property>]
    let ``abs(v) % k equals abs(v % k)`` v (NonZeroInt k) = 
        (abs v) % k = abs(v % k) + 1


    [<Property>]
    let DivMod (x:int) (y:int) = 
        y <> 0 ==> lazy (let (d,m) = divMod x y in d*y + m = x)
    
    [<Property>]    
    let MkStdGen (IntWithMinMax seed) =
        within 1000 <| lazy (let (StdGen (s1,s2)) = mkStdGen (int64 seed) in s1 > 0 && s2 > 0 (*todo:add check*) ) //check for bug: hangs when seed = min_int
        //|> collect seed


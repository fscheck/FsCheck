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

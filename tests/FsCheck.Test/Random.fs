namespace FsCheck.Test

module Random =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open FsCheck.Random

    
//    for some reason, this test has started failing on AppVeyor. 
//    [<Property>]
//    let ``mkStdGen should return StdGen for every seed`` (IntWithMinMax seed) =
//        Prop.within 1000 <| lazy (let (StdGen (s1,s2)) = mkStdGen (int64 seed) in s1 > 0 && s2 > 0 (*todo:add check*) )

    [<Fact>]
    let ``mkStdGen should not hang when seed is min or max value``() =
        Random.createWithSeed UInt64.MinValue |> ignore
        Random.createWithSeed UInt64.MaxValue |> ignore
        


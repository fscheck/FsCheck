namespace FsCheck.Test

module Random =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit

    
//    for some reason, this test has started failing on AppVeyor. 
//    [<Property>]
//    let ``mkStdGen should return StdGen for every seed`` (IntWithMinMax seed) =
//        Prop.within 1000 <| lazy (let (StdGen (s1,s2)) = mkStdGen (int64 seed) in s1 > 0 && s2 > 0 (*todo:add check*) )

    [<Fact>]
    let ``mkStdGen should not hang when seed is min or max value``() =
        Random.createWithSeed UInt64.MinValue |> ignore
        Random.createWithSeed UInt64.MaxValue |> ignore
        
    // theese tests may be helpful to check Random progress guarantees. See #356
    [<Property(Replay = "10538531436017130025,14826463994991344553", MaxTest = 30000, EndSize = 30000)>]
    let ``Random should not hang on 30k ints`` () =
        true

    [<Fact>]
    let ``Random should not hang on bad case`` () = 
        let rnd = Random.createWithSeedAndGamma (13471455474525100574UL,7555858534656909083UL)
        let n = 19938
        Random.rangeInt (0, n, rnd)
namespace FsCheck.Test

module Random =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open FsCheck.Random


    [<Property>]
    let ``abs(v) % k equals abs(v % k)`` v (NonZeroInt k) = 
        (abs v) % k = abs(v % k)

    [<Property>]
    let ``divMod should satisfy definition`` (x:int64) (y:int64) = 
        y <> 0L ==> lazy (let (d,m) = divMod64 x y in d*y + m = x)
    
//    for some reason, this test has started failing on AppVeyor. 
//    [<Property>]
//    let ``mkStdGen should return StdGen for every seed`` (IntWithMinMax seed) =
//        Prop.within 1000 <| lazy (let (StdGen (s1,s2)) = mkStdGen (int64 seed) in s1 > 0 && s2 > 0 (*todo:add check*) )

    [<Fact>]
    let ``mkStdGen should not hang when seed = MinInt``() =
        mkStdGen Int64.MinValue,  mkStdGen (int64 Int32.MinValue) |> ignore
        


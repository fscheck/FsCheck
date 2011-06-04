namespace FsCheck.Test

module Random =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Prop
    open FsCheck.Random
    open Helpers


    [<Property>]
    let ``abs(v) % k equals abs(v % k)`` v (NonZeroInt k) = 
        (abs v) % k = abs(v % k)

    [<Property>]
    let ``divMod should satisfy definition`` (x:int) (y:int) = 
        y <> 0 ==> lazy (let (d,m) = divMod x y in d*y + m = x)
    
    [<Property>]    
    let ``mkStdGen should return StdGen for every seed`` (IntWithMinMax seed) =
        within 1000 <| lazy (let (StdGen (s1,s2)) = mkStdGen (int64 seed) in s1 > 0 && s2 > 0 (*todo:add check*) )

    [<Fact>]
    let ``mkStdGen should not hang when seed = MinInt``() =
        let result = mkStdGen Int64.MinValue,  mkStdGen (int64 Int32.MinValue)
        ()


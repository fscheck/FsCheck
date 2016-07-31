namespace FsCheck.Test

module Random =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit

    
    [<Property>]
    let ``mkStdGen should return Rnd for every seed`` seed =
        Random.createWithSeed seed |> ignore

    [<Fact>]
    let ``mkStdGen should not hang when seed is min or max value``() =
        Random.createWithSeed UInt64.MinValue |> ignore
        Random.createWithSeed UInt64.MaxValue |> ignore
        


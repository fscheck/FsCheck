namespace FsCheck.Test

open Xunit

open FsCheck
open FsCheck.FSharp

open Swensen.Unquote

module ArbMapTest =

    // note this is a dangerous test to write, as memoization on the default map
    // is globally observable side-effect. It is especially important that this works
    // for ArbMap.Default however, so in this case the tradeoff is worth it - it is 
    // otherwise hard to detect, giving arbitrary slowdowns depending on use case.

    [<Fact>]
    let ``should memoize concrete types``() =

        ArbMap.defaults |> ArbMap.generate<int> |> ignore
        test <@ Array.contains typeof<int> (ArbMap.defaults :?> ArbMap).MemoizedInstances @>

    [<Fact>]
    let ``should memoize generic types``() =
        ArbMap.defaults |> ArbMap.generate<list<int>> |> ignore
        test <@ Array.contains typeof<list<int>> (ArbMap.defaults :?> ArbMap).MemoizedInstances @>

    [<Fact>]
    let ``should memoize reflectively generated types``() =
        ArbMap.defaults |> ArbMap.generate<int*char*string> |> ignore
        test <@ Array.contains typeof<int*char*string> (ArbMap.defaults :?> ArbMap).MemoizedInstances @>

    type Reco = { one: int; two: int; }
    type TypeMerge = 
        static member Int() = gen { return 0 } |> Arb.fromGen

    let private compareForType<'a when 'a: equality> left right sampleSize = 
            (left |> ArbMap.generate<'a> |> Gen.sample sampleSize) =! (right |> ArbMap.generate<'a> |> Gen.sample sampleSize)
    [<Fact>]
    let ``mergeFactory behaves the same as mergeWith`` () =
        let arbFac = ArbMap.defaults |> ArbMap.mergeFactory TypeMerge.Int
        let arbTyp = ArbMap.defaults |> ArbMap.mergeWith<TypeMerge>
        
        compareForType<int> arbFac arbTyp 5
        compareForType<Reco> arbFac arbTyp 5

    [<Fact>]
    let ``ensure lastest ArbMap is takes priority as config parameter`` () =
        let constInt = 0
        let arbFac = 
            ArbMap.defaults 
            |> ArbMap.mergeMapFactory (fun map -> 
                map |> ArbMap.generate<int> 
                |> Gen.map (fun i -> {one = i; two = i})
                |> Arb.fromGen)
            |> ArbMap.mergeFactory (fun () -> gen {return constInt} |> Arb.fromGen)

        let n = 5
        let expected = Array.init n (fun i -> {one = constInt; two =constInt })
        let actual = arbFac |> ArbMap.generate<Reco> |> Gen.sample n
        expected =! actual
        
        
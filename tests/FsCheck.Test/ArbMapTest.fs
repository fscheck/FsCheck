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


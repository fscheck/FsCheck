namespace FsCheck.Test

module Helpers = 

    open System
    open FsCheck

    let sample n = Gen.sample 1000 n

    let sample1 gn = sample 1 gn |> List.head
    
    let isIn l elem = List.exists ((=) elem) l

    open global.Xunit
    open System.Threading
    open System.Threading.Tasks

    [<Fact>]
    let ``memoize is thread-safe``() =
        let f (a: int) = Thread.Sleep 100; a
        let memoized = Common.memoize f
        Array.init 100 (fun _ -> Action(fun _ -> memoized 1 |> ignore))
        |> Parallel.Invoke

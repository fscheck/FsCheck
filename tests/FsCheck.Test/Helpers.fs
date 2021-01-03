namespace FsCheck.Test

[<assembly: Xunit.CollectionBehavior(Xunit.CollectionBehavior.CollectionPerAssembly)>]
do()

module Helpers = 

    open System
    open FsCheck
    open FsCheck.FSharp

    let sample n = Gen.sampleWithSize 1000 n

    let sample1 gn = sample 1 gn |> Seq.head
    
    let isIn l elem = List.exists ((=) elem) l

    let assertTrue pred = if not pred then failwith "assertion failure"

    open global.Xunit
    open System.Threading
    open System.Threading.Tasks

    [<Fact>]
    let ``memoize is thread-safe``() =
        let f (a: int) = Thread.Sleep 100; a
        let memoized = Internals.Common.memoize f
        Array.init 100 (fun _ -> Action(fun _ -> memoized 1 |> ignore))
        |> Parallel.Invoke

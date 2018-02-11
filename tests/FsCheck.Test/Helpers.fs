namespace FsCheck.Test

[<assembly: Xunit.CollectionBehavior(Xunit.CollectionBehavior.CollectionPerAssembly)>]
do()

module Helpers = 

    open System
    open FsCheck

    let sample n = Gen.sampleWithSize 1000 n

    let sample1 gn = sample 1 gn |> Seq.head

    let assertTrue pred = if not pred then failwith "assertion failure"

    let assertShrinksFrom c (gen:Gen<'T>) foreachShrink =
        for i in 1..c do
            let from,shrinks = Gen.toShrink gen
            let shrinkArr = shrinks |> Shrink.toSeq |> Seq.toArray
            shrinkArr |> Seq.iter (foreachShrink from)

    let assertShrinks<'T> foreachShrink =
        assertShrinksFrom 10 Arb.generate<'T> foreachShrink

    open global.Xunit
    open System.Threading
    open System.Threading.Tasks

    [<Fact>]
    let ``memoize is thread-safe``() =
        let f (a: int) = Thread.Sleep 100; a
        let memoized = Common.memoize f
        Array.init 100 (fun _ -> Action(fun _ -> memoized 1 |> ignore))
        |> Parallel.Invoke

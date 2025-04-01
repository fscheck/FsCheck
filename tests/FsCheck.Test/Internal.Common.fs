module FsCheck.Test.Internals

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

[<Property>]
let ``memoized functions return the same value of the original function`` (f: string -> int, a: string) =

    let memoized = FsCheck.Internals.Common.memoize f

    let result = f a
    let resultFromMemoized = memoized a

    test <@ resultFromMemoized = result @>

[<Property>]
let ``memoized functions are invoked only once`` (f: string -> int, a: string, times: PositiveInt) =
    let mutable numberOfInvocations = 0

    let countAndInvoke a =
        numberOfInvocations <- numberOfInvocations + 1
        f a

    let memoized = FsCheck.Internals.Common.memoize countAndInvoke

    [ 0..times.Get ]
    |> Seq.iter (fun _ -> memoized a |> ignore)


    test <@ numberOfInvocations = 1 @>

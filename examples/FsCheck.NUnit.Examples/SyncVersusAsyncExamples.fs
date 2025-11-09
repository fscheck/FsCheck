namespace FsCheck.NUnit.Examples

open FsCheck.FSharp
open FsCheck.NUnit

module SyncVersusAsyncExamples =
    let private doSomethingAsync () = async { return () }

    [<Property>]
    let ``Sync - should pass`` (b: bool) =
        b = b |> Prop.label "b = b"

    [<Property>]
    let ``Sync - should fail`` (b: bool) =
        b = not b |> Prop.label "b = not b"

    [<Property>]
    let ``Async - should pass`` (b: bool) =
        async {
            do! doSomethingAsync ()
            return b = b |> Prop.label "b = b"
        }

    [<Property>]
    let ``Async - should fail`` (b : bool) =
        async {
            do! doSomethingAsync ()
            return b = not b |> Prop.label "b = not b"
        }

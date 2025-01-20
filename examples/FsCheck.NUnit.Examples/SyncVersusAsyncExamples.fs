namespace FsCheck.NUnit.Examples

open FsCheck.FSharp
open FsCheck.NUnit

module SyncVersusAsyncExamples =
    let private doSomethingAsync () = async { return () }

    [<Property>]
    let ``Sync - should pass`` b =
        b = b |> Prop.label "b = b"

    [<Property>]
    let ``Sync - should fail`` b =
        b = not b |> Prop.label "b = not b"

    [<Property>]
    let ``Async - should pass`` b =
        async {
            do! doSomethingAsync ()
            return b = b |> Prop.label "b = b"
        }

    [<Property>]
    let ``Async - should fail`` b =
        async {
            do! doSomethingAsync ()
            return b = not b |> Prop.label "b = not b"
        }

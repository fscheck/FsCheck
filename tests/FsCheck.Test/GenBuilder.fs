namespace FsCheck.Test

open System
open Xunit

module GenBuilder =

    open FsCheck.FSharp
    open Helpers
    open Swensen.Unquote.Assertions

    [<Fact>]
    let ``for loop``() =
        let actual = 
            gen {
                let result = Array.zeroCreate 10
                for i in 0..9 do
                    let! e = Gen.constant i
                    result.[i] <- e
                return result
            }
            |> sample1
        test <@ [|0..9|] = actual @>

    [<Fact>]
    let ``while loop``() =
        let actual = 
            gen {
                let result = Array.zeroCreate 10
                let mutable i = 0
                while i < 10 do
                    let! e = Gen.constant i
                    result.[i] <- e
                    i <- i + 1
                return result
            }
            |> sample1
        test <@ [|0..9|] = actual @>

    [<Fact>]
    let ``use``() =
        let mutable disposeCalled = false
        let disposable() = 
            { new IDisposable with
                override _.Dispose() = disposeCalled <- true
            }
        let actual = 
            gen {
                use! d = Gen.fresh disposable
                return d
            } |> sample1
        assertTrue disposeCalled

    [<Fact>]
    let ``try with``() =
        let actual =
            gen {
                try
                    let! a = Gen.constant 15
                    invalidOp <| sprintf "%i" a
                    return a
                with :? InvalidOperationException as e ->
                    test <@ "15" = e.Message @>
                    return -1
            }
            |> sample1
        test <@ actual = -1 @>

    [<Fact>]
    let ``try finally``() =
        let mutable finallyHandlerCalled = false
        let actual =
            gen {
                try
                    let! a = Gen.constant 15
                    return a
                finally
                    finallyHandlerCalled <- true
            }
            |> sample1
        assertTrue finallyHandlerCalled

    [<Fact>]
    let ``let and``() =
        let actual =
            gen {
                let! a = Gen.constant 5
                and! b = Gen.constant 15
                and! c = Gen.constant 25
                and! d = Gen.constant 35
                return a + b + c + d
            }
            |> sample1
        test <@ 80 = actual @>
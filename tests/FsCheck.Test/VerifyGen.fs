namespace FsCheck.Test.Verify


open Xunit
open VerifyXunit

open FsCheck
open FsCheck.FSharp
open System.Threading
open VerifyTests
open System.Reflection
open System

[<UsesVerify>]
module VerifyGen =

    let seed = Random.CreateWithSeed(9854927542UL)
    let size = 50
    let nbSamples = 20
    
    let sample g = g |> Gen.sampleWithSeed seed size nbSamples

    let verify (anything:'T) =
        // Verify doesn't return a Task, exactly, it returns an awaitable.
        // Sadly I couldn't find a less heavy-handed way of doing this.
        let awaiter = Verifier.Verify<'T>(anything)
                        .UseDirectory("Verified")
                        .ModifySerialization(fun t -> t.DontScrubDateTimes())
                        .GetAwaiter()
        async {
            use handle = new SemaphoreSlim(0)
            awaiter.OnCompleted(fun () -> ignore (handle.Release()))
            let! _ = handle.AvailableWaitHandle   |> Async.AwaitWaitHandle
            return awaiter.GetResult() 
        } |> Async.StartAsTask

    let verifyGen (gen:Gen<'T>) =
        gen
        |> sample 
        |> verify

    [<Fact>]
    let ``choose(-100,100)``() =
        Gen.choose(-100,100)
        |> verifyGen

    [<Fact>]
    let ``choose64(-100,100)``() =
        Gen.choose64(-100L,100L)
        |> verifyGen

    [<Fact>]
    let ``arrayOf choose(-10,10)``() =
        Gen.choose(-10,10)
        |> Gen.arrayOf
        |> verifyGen

    [<Fact>]
    let ``listOf choose(-10,10)``() =
        Gen.choose(-10,10)
        |> Gen.listOf
        |> verifyGen

    [<Fact>]
    let ``Double``() =
        ArbMap.defaults
        |> ArbMap.generate<Double>
        |> verifyGen

    [<Fact>]
    let ``String``() =
        ArbMap.defaults
        |> ArbMap.generate<String>
        |> verifyGen

    [<Fact>]
    let ``DateTimeOffset``() =
        ArbMap.defaults
        |> ArbMap.generate<DateTimeOffset>
        |> verifyGen

    [<Fact>]
    let ``Map int,char``() =
        ArbMap.defaults
        |> ArbMap.generate<Map<int,char>>
        |> verifyGen

        
// without this, Verify refuses to work.
[<AssemblyMetadataAttribute("Verify.ProjectDirectory", "anything that is unlikely to show up in values")>]
do ()
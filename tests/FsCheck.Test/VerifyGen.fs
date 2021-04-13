namespace FsCheck.Test.Verify

open System
open System.Reflection
open System.Threading

open Xunit
open VerifyXunit

open FsCheck
open FsCheck.FSharp
open Newtonsoft.Json

[<UsesVerify>]
module VerifyGen =

    let seed = Random.CreateWithSeed(9854927542UL)
    let size = 50
    let nbSamples = 20
    
    let sample g = g |> Gen.sampleWithSeed seed size nbSamples

    let sampleSmall g = g |> Gen.sampleWithSeed seed (size/2) (nbSamples/2)

    let verify (anything:'T) =
        // Verify doesn't return a Task, exactly, it returns an awaitable.
        // But xunit requires a Task back. In C# you can just await it.
        // I couldn't find a less heavy-handed way of doing the same in F#.
        let awaiter = Verifier.Verify<'T>(anything)
                        .UseDirectory("Verified")
                        .ModifySerialization(fun t -> 
                            t.DontScrubDateTimes()
                            t.DontIgnoreEmptyCollections()
                            t.DontIgnoreFalse())
                        .AddExtraSettings(fun t ->
                            t.NullValueHandling <- NullValueHandling.Include)
                        .GetAwaiter()
        async {
            use handle = new SemaphoreSlim(0)
            awaiter.OnCompleted(fun () -> ignore (handle.Release()))
            let! _ = handle.AvailableWaitHandle |> Async.AwaitWaitHandle
            return awaiter.GetResult() 
        } |> Async.StartAsTask

    type ShrinkVerify<'T> =
        { Original: 'T // original value that is being shrunk
          Success: array<'T> // array of shrinks, assuming all shrinks succeed
          Fail: array<'T> // array of shrinks, assuming all shrinks fail
        }

    let verifyArb (arb:Arbitrary<'T>) =
        let samples = arb |> Arb.unArb |> sampleSmall
        let toVerify =
            samples
            |> Array.map (Internals.Shrink.sample 
                          >> (fun v -> { Original = v.Original 
                                         Success = v.Success
                                         Fail = v.Fail }))
        toVerify |> verify

    [<Fact>]
    let ``choose(-100,100)``() =
        Arb.choose(-100,100)
        |> verifyArb

    [<Fact>]
    let ``choose64(-100,100)``() =
        Arb.choose64(-100L,100L)
        |> verifyArb

    [<Fact>]
    let ``array of choose(-10,10)``() =
        Arb.choose(-10,10)
        |> Arb.array
        |> verifyArb

    [<Fact>]
    let ``list of choose(-10,10)``() =
        Arb.choose(-10,10)
        |> Arb.list
        |> verifyArb

    [<Fact>]
    let ``Int32``() =
        ArbMap.defaults
        |> ArbMap.arbitrary<Int32>
        |> verifyArb

    [<Fact>]
    let ``Option of bool``() =
        ArbMap.defaults
        |> ArbMap.arbitrary<option<bool>>
        |> verifyArb

    [<Fact>]
    let ``Double``() =
        ArbMap.defaults
        |> ArbMap.arbitrary<Double>
        |> verifyArb

    [<Fact>]
    let ``Array of Int32``() =
        ArbMap.defaults
        |> ArbMap.arbitrary<int[]>
        |> verifyArb

    [<Fact>]
    let ``List of Int32``() =
        ArbMap.defaults
        |> ArbMap.arbitrary<list<int>>
        |> verifyArb

    [<Fact>]
    let ``String``() =
        ArbMap.defaults
        |> ArbMap.arbitrary<String>
        |> verifyArb

    //[<Fact>]
    //let ``DateTimeOffset``() =
    //    ArbMap.defaults
    //    |> ArbMap.arbitrary<DateTimeOffset>
    //    |> verifyArb

    [<Fact>]
    let ``Map int,char``() =
        ArbMap.defaults
        |> ArbMap.arbitrary<Map<int,char>>
        |> verifyArb

        
// without this, attribute Verify refuses to work.
// Also, it automatically replaces anything that looks like the value with {ProjectDirectory},
// which we also never want.
[<AssemblyMetadataAttribute("Verify.ProjectDirectory", "anything that is unlikely to show up in values")>]
do ()
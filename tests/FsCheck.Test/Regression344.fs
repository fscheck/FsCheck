namespace FsCheck.Test

// see https://github.com/fscheck/FsCheck/issues/344
// Consider a test, where the first iteration failed, but all subsequent shrinks passed.
// Each successfull shrink caused the stackframe to grow by 2-3 frames, causing a stackoverflow.
module Regression344 =
    
    open FsCheck
    open global.Xunit

    open System.Diagnostics
    open System.Threading

    type IntWrapper = IntWrapper of int
      with
        member x.I =
            let (IntWrapper i) = x
            i

    type MyGenerators =
        static member FilterModel =
            { new Arbitrary<IntWrapper>() with
                // start with 1 ...
                override __.Generator =
                    Gen.constant (IntWrapper 1)
                // ... and count upwards with each shrink
                override __.Shrinker _ =
                    Seq.initInfinite (fun i -> IntWrapper(i + 2))
            }

    [<Fact>]
    let ``Shrinks dont cause a stackoverflow``() =

        // so this is really ugly and hacky, but I don't know how to better signal abortion from inside the callback.
        // throwing an exception doesn't work, since it then just assumes the test failed, and tries to shrink again ...
        let mutable tooManyFrames = false
            
        // run this on another thread, and abort it manually
        let thread2 = Thread(fun () ->
            let config = { Config.Quick with Arbitrary = [ typeof<MyGenerators> ]  }
            Check.One(config, fun (x:IntWrapper) -> 
                // fail the first iteration
                if x.I = 1 then
                    false
                else
                    if x.I >= 200 then
                        // after 200 iterations, check the frame count and abort
                        let st = new StackTrace()
                        tooManyFrames <- st.FrameCount > 200
                        Thread.CurrentThread.Abort()
                    true))
        thread2.Start()
        thread2.Join()
        if tooManyFrames then failwith "too many frames, possible stackoverflow detected"

(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **  
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

open System
open System.Runtime.CompilerServices

///Configure the test run.
type Configuration() =
    let mutable maxTest = Config.Quick.MaxTest
    let mutable maxFail = Config.Quick.MaxFail
    let mutable name = Config.Quick.Name
    let mutable every = Config.Quick.Every
    let mutable everyShrink = Config.Quick.EveryShrink
    let mutable startSize = Config.Quick.StartSize
    let mutable endSize = Config.Quick.EndSize
    let mutable quietOnSuccess = Config.Quick.QuietOnSuccess
    let mutable runner = Config.Quick.Runner
    let mutable replay = Config.Quick.Replay

    ///The maximum number of tests that are run.
    member __.MaxNbOfTest with get() = maxTest and set(v) = maxTest <- v

    ///The maximum number of tests where values are rejected
    member __.MaxNbOfFailedTests with get() = maxFail and set(v) = maxFail <- v

    ///Name of the test.
    member __.Name with get() = name and set(v) = name <- v

    ///What to print when new arguments args are generated in test n
    member __.Every with get() = new Func<int,obj array,string>(fun i arr -> every i (Array.toList arr)) 
                    and set(v:Func<int,obj array,string>) = every <- fun i os -> v.Invoke(i,List.toArray os)

    ///What to print every time a counter-example is succesfully shrunk
    member __.EveryShrink with get() = new Func<obj array,string>(Array.toList >> everyShrink)
                          and set(v:Func<obj array,string>) = everyShrink <- fun os -> v.Invoke(List.toArray os)

    ///The size to use for the first test.
    member __.StartSize with get() = startSize and set(v) = startSize <- v

    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member __.EndSize with get() = endSize and set(v) = endSize <- v

    ///If set, suppresses the output from the test if the test is successful.
    member __.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v

    ///A custom test runner, e.g. to integrate with a test framework like xUnit or NUnit. 
    member __.Runner with get() = runner and set(v) = runner <- v

    ///If set, the seed to use to start testing. Allows reproduction of previous runs.
    member __.Replay 
        with get() = (match replay with None -> Unchecked.defaultof<Random.StdGen> | Some s -> s)
        and set(v) = match box v with null -> () | _ -> replay <- Some v

    member internal __.ToConfig() =
        { MaxTest = maxTest
          MaxFail = maxFail 
          Name = name
          Every = every
          EveryShrink = everyShrink
          StartSize = startSize
          EndSize = endSize
          QuietOnSuccess = quietOnSuccess
          Runner = runner
          Replay = replay
          Arbitrary = []
        }


[<AbstractClass;Sealed;Extension>]
type CheckExtensions =
    [<Extension>]
    static member Check(property:Property,config:Configuration) = Check.One(config.ToConfig(),property)
    [<Extension>]
    static member QuickCheck(property:Property) = Check.Quick property
    [<Extension>]
    static member QuickCheck(property:Property, testName:string) = Check.Quick(testName, property)
    [<Extension>]
    static member QuickCheckThrowOnFailure(property:Property) = Check.QuickThrowOnFailure property
    [<Extension>]
    static member VerboseCheck(property:Property) = Check.Verbose property
    [<Extension>]
    static member VerboseCheck(property:Property, testName:string) = Check.Verbose(testName, property)
    [<Extension>]
    static member VerboseCheckThrowOnFailure(property:Property) = Check.VerboseThrowOnFailure property

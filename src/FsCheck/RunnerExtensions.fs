namespace FsCheck

open System
open System.Runtime.CompilerServices

[<AbstractClass;Sealed;Extension>]
type ConfigExtensions =
    [<Extension>]
    ///Returns a new Config with specified Every function
    static member WithEvery(config:Config, every:Func<int,obj array,string>) =
        config.WithEvery(fun i os -> every.Invoke(i,List.toArray os))

    [<Extension>]
    ///Returns a new Config with specified EveryShrink function
    static member WithEveryShrink(config:Config, everyShrink:Func<obj array,string>) =
        config.WithEveryShrink(fun os -> everyShrink.Invoke(List.toArray os))

    [<Extension>]
    ///Returns a new Config with specified Replay
    static member WithReplay(config: Config, seed:uint64, gamma:uint64, size:int) =
        config.WithReplay(Some { Rnd = Random.createWithSeedAndGamma (seed,gamma); Size = if size <= 0 then None else Some size })

    [<Extension>]
    ///Returns a new Config with specified Replay
    static member WithReplay(config: Config, seed:uint64, gamma:uint64) =
        config.WithReplay(seed, gamma, -1)

    [<Extension>]
    ///Returns a new Config with no Replay
    static member WithNoReplay(config: Config) =
        config.WithReplay(None)
        
    [<Extension>]
    ///Returns a new Config with specified ParallelRunConfig
    static member WithParallelRunConfig(config:Config, parallelRunConfig: ParallelRunConfig) =
        config.WithParallelRunConfig(Some parallelRunConfig)

    [<Extension>]
    ///Returns a new Config with no ParallelRunConfig
    static member WithNoParallelRunConfig(config:Config) =
        config.WithParallelRunConfig(None)
    

[<AbstractClass;Sealed;Extension>]
type CheckExtensions =
    [<Extension>]
    static member Check(property:Property,config:Config) = Check.One(config,property)
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

namespace FsCheck.Fluent

open System
open System.Runtime.CompilerServices

open FsCheck
open FsCheck.FSharp

[<Extension; Sealed; AbstractClass>]
type ArbMap private () =

    static member Default = ArbMap.defaults

    [<Extension>]
    static member Merge<'TArb>(map: IArbMap) = ArbMap.mergeWith<'TArb> map

    [<Extension>]
    static member Merge(map: IArbMap, instancesType: Type) = ArbMap.mergeWithType instancesType map

    [<Extension>]
    static member MergeArb<'T>(map: IArbMap, arb:Arbitrary<'T>) = ArbMap.mergeArb arb map

    [<Extension>]
    static member MergeArbFactory<'T>(map: IArbMap, factory:Func<IArbMap,Arbitrary<'T>>) = 
        map.MergeFactory(factory) 

    [<Extension>]
    static member MergeArbFactory<'T, 'U>(map: IArbMap, factory:Func<Arbitrary<'T>,Arbitrary<'U>>) = 
        map.MergeFactory(factory)

    [<Extension>]
    static member MergeArbFactory<'T>(map: IArbMap, factory:Func<Arbitrary<'T>>) = 
        map.MergeFactory(fun () -> factory.Invoke())

    [<Extension>]
    static member GeneratorFor<'T>(map: IArbMap) = ArbMap.generate<'T> map 
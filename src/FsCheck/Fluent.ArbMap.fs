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
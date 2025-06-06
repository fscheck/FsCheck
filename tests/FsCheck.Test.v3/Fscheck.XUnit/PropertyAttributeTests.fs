﻿namespace Fscheck.Test.FsCheck.XUnit.PropertyAttribute

open System.Threading.Tasks
open FsCheck.FSharp
open FsCheck.Xunit
open Xunit

type AttributeLevel =
| Assembly
| ClassOrModule
| NestedClassOrModule
| MethodOrProperty

type AttributeLevel_Assembly() =
    static member Generator = 
        Assembly
        |> Gen.constant
        |> Arb.fromGen

type AttributeLevel_ClassOrModule() =
    static member Generator = 
        ClassOrModule
        |> Gen.constant
        |> Arb.fromGen

type AttributeLevel_MethodOrProperty() =
    static member Generator =
        MethodOrProperty
        |> Gen.constant
        |> Arb.fromGen

type AttributeLevel_NestedClassOrModule() =
    static member Generator =
        NestedClassOrModule
        |> Gen.constant
        |> Arb.fromGen

[<assembly: Properties(Arbitrary = [| typeof<AttributeLevel_Assembly> |])>]
do()

module ``when module does not have properties attribute``=
    [<Property>]
    let ``then the assembly attribute should be used`` = function
    | Assembly -> true
    | _ -> false

    [<Property(Arbitrary = [| typeof<AttributeLevel_MethodOrProperty>|])>]
    let ``then the property attribute takes precient`` = function
    | MethodOrProperty -> true
    | _ -> false

[<Properties(Arbitrary = [|typeof<AttributeLevel_ClassOrModule>|])>]
module ``when module has properties attribute`` =

    [<Property>]
    let ``then the module's property takes precident`` = function
    | ClassOrModule -> true
    | _ -> false

    [<Property(Arbitrary = [| typeof<AttributeLevel_MethodOrProperty>|])>]
    let ``then the property attribute takes precient`` = function
    | MethodOrProperty -> true
    | _ -> false

    [<Properties(Arbitrary = [|typeof<AttributeLevel_NestedClassOrModule>|])>]
    module ``and there is and nested module`` =
        [<Property>]
        let ``then the nested module's property takes precident`` = function
        | NestedClassOrModule -> true
        | _ -> false


module ``when type implements IAsyncLifetime`` =
    type Issue657() =

        let mutable executed = false;

        interface IAsyncLifetime with
            member _.InitializeAsync() =

                async {
                    do! Async.Sleep 300
                    executed <- true
                    return ()
                }
                |> Async.StartAsTask
                :> Task
                |> ValueTask
                
            member _.DisposeAsync() = ValueTask()

        [<Property(MaxTest = 1)>]
        member this.``then InitializeAsync() is invoked``() =
            executed = true

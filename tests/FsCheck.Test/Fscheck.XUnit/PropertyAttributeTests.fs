namespace Fscheck.Test.FsCheck.XUnit.PropertyAttribute

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

            member _.DisposeAsync() = Task.CompletedTask

        [<Property(MaxTest = 1)>]
        member this.``then InitializeAsync() is invoked``() =
            executed = true

/// Reproduction test for GitHub issue: Lifetime problem with Xunit: InvalidOperationException: There is no currently active test.
/// This test class verifies that mixing Property and Fact tests with ITestOutputHelper doesn't cause lifetime issues.
module ``when mixing Property and Fact tests with ITestOutputHelper`` =
    open Xunit.Abstractions

    type TestOutputHelperLifetimeTests(output: ITestOutputHelper) =

        [<Property>]
        member _.``Property test with parameter writes to output`` (x: int) =
            output.WriteLine($"Property test with parameter: {x}")
            true

        [<Fact>]
        member _.``Fact test writes to output`` () =
            output.WriteLine("Fact test")

        [<Property>]
        member _.``Property test with string parameter writes to output`` (s: string) =
            let str = if isNull s then "null" else s
            output.WriteLine($"Property test with string: {str}")
            true

        [<Fact>]
        member _.``Another fact test writes to output`` () =
            output.WriteLine("Another fact test")

        /// This test specifically exercises the Every and EveryShrink callbacks by enabling Verbose mode.
        /// These callbacks capture the TestOutputHelper in closures, which was the root cause of the lifetime issue.
        [<Property(Verbose = true, MaxTest = 5)>]
        member _.``Verbose property test exercises Every and EveryShrink callbacks`` (x: int) (y: int) =
            output.WriteLine($"Verbose mode test: x={x}, y={y}")
            true

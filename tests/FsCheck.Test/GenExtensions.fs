namespace FsCheck.Test

module GenExtensions =

    open Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Fluent
    open Swensen.Unquote
    open System

    [<Fact>]
    let Select () =
        raises<ArgumentNullException> 
            <@ Arb.generate<int>.Select(selector=null) @>

    [<Fact>]
    let Where () =
        raises<ArgumentNullException> 
            <@ Arb.generate<float>.Where(predicate=null) @>

    [<Fact>]
    let SelectMany () =
        raises<ArgumentNullException> 
            <@ Arb.generate<string>.SelectMany(selector=null) @>
    
    [<Fact>]
    let ``SelectMany binder``() =
        let act =
            lazy Arb.generate<string>.SelectMany(
                selector=null, 
                resultSelector=new Func<string, string, string>((+)))

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure

    [<Fact>]
    let ``SelectMany mapper``() =
        let act = 
            lazy Arb.generate<string>.SelectMany (
                new Func<string, Gen<string>> (fun s -> Gen.Constant(s)),
                resultSelector=null)

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure

    [<Fact>]
    let Zip () =
        let act =
            lazy Arb.generate<TimeSpan>.Zip(
                    Arb.generate<DateTimeOffset>, 
                    resultSelector=null)

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure
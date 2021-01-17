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
            <@ GenExtensions.Select (Arb.generate<int>, selector=null) @>

    [<Fact>]
    let Where () =
        raises<ArgumentNullException> 
            <@ GenExtensions.Where (Arb.generate<float>, predicate=null) @>

    [<Fact>]
    let SelectMany () =
        raises<ArgumentNullException> 
            <@ GenExtensions.SelectMany (Arb.generate<string>, selector=null) @>
    
    [<Fact>]
    let ``SelectMany binder``() =
        let act =
            lazy GenExtensions.SelectMany (
                Arb.generate<string>, 
                selector=null, 
                resultSelector=new Func<string, string, string>((+)))

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure

    [<Fact>]
    let ``SelectMany mapper``() =
        let act = 
            lazy GenExtensions.SelectMany (
                Arb.generate<string>,
                new Func<string, Gen<string>> (fun s -> Gen.Constant(s)),
                resultSelector=null)

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure

    [<Fact>]
    let Zip () =
        let act =
            lazy GenExtensions.Zip (
                Arb.generate<TimeSpan>, 
                Arb.generate<DateTimeOffset>, 
                resultSelector=null)

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure
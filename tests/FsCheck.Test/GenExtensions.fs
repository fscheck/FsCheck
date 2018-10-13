namespace FsCheck.Test

module GenExtensions =

    open Xunit
    open FsCheck
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
            <@ GenExtensions.SelectMany (Arb.generate<string>, f=null) @>
    
    [<Fact>]
    let ``SelectMany binder``() =
        let act =
            lazy GenExtensions.SelectMany (
                Arb.generate<string>, 
                f=null, 
                select=new Func<string, string, string>(fun x y -> x + y))

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure

    [<Fact>]
    let ``SelectMany mapper``() =
        let act = 
            lazy GenExtensions.SelectMany (
                Arb.generate<string>, 
                new Func<string, Gen<string>> (Gen.constant),
                select=null)

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

    [<Fact>]
    let Zip3 () =
        raises<ArgumentNullException> 
            <@ GenExtensions.Zip (
                Arb.generate<char>,
                Arb.generate<string>,
                Arb.generate<byte>,
                resultSelector=null) @>
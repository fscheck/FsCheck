namespace FsCheck.Test

module GenExtensions =

    open Xunit
    open FsCheck
    open FsCheck.Fluent
    open Swensen.Unquote
    open System

    [<Fact>]
    let Select () =
        raises<ArgumentNullException> 
            <@ Fluent.Gen.Choose(0,5).Select(selector=null) @>

    [<Fact>]
    let Where () =
        raises<ArgumentNullException> 
            <@ Fluent.Gen.Choose(0,5).Where(predicate=null) @>

    [<Fact>]
    let SelectMany () =
        raises<ArgumentNullException> 
            <@ ArbMap.Default.GeneratorFor<string>().SelectMany(selector=null) @>
    
    [<Fact>]
    let ``SelectMany binder``() =
        let act =
            lazy ArbMap.Default.GeneratorFor<string>().SelectMany(
                selector=null, 
                resultSelector=new Func<string, string, string>((+)))

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure

    [<Fact>]
    let ``SelectMany mapper``() =
        let act = 
            lazy ArbMap.Default.GeneratorFor<string>().SelectMany (
                new Func<string, Gen<string>> (fun s -> Gen.Constant(s)),
                resultSelector=null)

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure

    [<Fact>]
    let Zip () =
        let act =
            lazy ArbMap.Default.GeneratorFor<TimeSpan>().Zip(
                    ArbMap.Default.GeneratorFor<TimeSpan>(), 
                    resultSelector=null)

        Prop.throws<ArgumentNullException, _> act
        |> Check.QuickThrowOnFailure
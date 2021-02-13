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
        let func = new Func<string, string, string>((+))
        raises<ArgumentNullException>
            <@ ArbMap.Default.GeneratorFor<string>().SelectMany(selector=null, resultSelector=func) @>

    [<Fact>]
    let ``SelectMany mapper``() =
        let func = new Func<string, Gen<string>> (fun s -> Gen.Constant(s))
        raises<ArgumentNullException>
            <@ ArbMap.Default.GeneratorFor<string>().SelectMany(func, resultSelector=null) @>

    [<Fact>]
    let Zip () =
        raises<ArgumentNullException>
            <@ ArbMap.Default.GeneratorFor<TimeSpan>().Zip(ArbMap.Default.GeneratorFor<TimeSpan>(), resultSelector=null) @>

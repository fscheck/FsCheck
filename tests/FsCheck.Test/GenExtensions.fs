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
    let Zip () =
        raises<ArgumentNullException> 
            <@ GenExtensions.Zip (
                Arb.generate<TimeSpan>, 
                Arb.generate<DateTimeOffset>, 
                resultSelector=null) @>

    [<Fact>]
    let Zip3 () =
        raises<ArgumentNullException> 
            <@ GenExtensions.Zip (
                Arb.generate<char>,
                Arb.generate<string>,
                Arb.generate<byte>,
                resultSelector=null) @>
namespace FsCheck.FSharp

///Combinators to build properties, which define the property to be tested, with some
///convenience methods to investigate the generated arguments and any found counter-examples.
[<RequireQualifiedAccess>]
module Prop =
    open FsCheck
    open Testable
    open System

    ///Quantified property combinator. Provide a custom test data generator to a property.
    [<CompiledName("ForAll")>]
    let forAll (arb:Arbitrary<'Value>) (body:'Value -> 'Testable) = forAll arb body

    ///Depending on the condition, return the first testable if true and the second if false.
    [<CompiledName("Given")>]
    let given condition (iftrue:'TestableIfTrue, ifFalse:'TestableIfFalse) = 
        if condition then property iftrue else property ifFalse

    ///Conditional property combinator. Resulting property holds if the given property holds whenever the condition does. See also operator:  'assertion ==> property'
    let filter condition (assertion : 'Testable) = given condition (assertion,property Res.rejected)

    ///Expect exception 't when executing p. So, results in success if an exception of the given type is thrown, 
    ///and a failure otherwise.
    [<CompiledName("Throws")>]
    let throws<'Exception, 'Testable when 'Exception :> exn> (p : Lazy<'Testable>) = 
       try 
           ignore p.Value
           Res.failedException (exn "Expected exception, none was thrown") 
       with 
           | :? 'Exception -> 
               Res.passed
           | e ->
               Res.failedException e
       |> property

    let private stamp str = 
        let add res = 
            match res with
            | ResultContainer.Value r -> { r with Stamp = str :: r.Stamp } |> Value
            | ResultContainer.Future t -> t.ContinueWith (fun (rt :Threading.Tasks.Task<Result>) -> 
                let r = rt.Result
                { r with Stamp = str :: r.Stamp }) |> Future
        Prop.mapResult add

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<CompiledName("Classify")>]
    let classify b name : ('Testable -> Property) = if b then stamp name else property

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<CompiledName("Trivial")>]
    let trivial b : ('Testable -> Property) = classify b "trivial"

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported, using sprintf "%A".
    [<CompiledName("Collect")>]
    let collect (v:'CollectedValue) : ('Testable -> Property) = stamp <| sprintf "%A" v

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<CompiledName("Label")>]
    let label l : ('Testable -> Property) = 
        let add res = 
            match res with
            | ResultContainer.Value r -> { r with Labels = Set.add l r.Labels } |> Value
            | ResultContainer.Future t -> t.ContinueWith (fun (rt :Threading.Tasks.Task<Result>) -> 
                let r = rt.Result
                { r with Labels = Set.add l r.Labels }) |> Future
        Prop.mapResult add

    /// Turns a testable type into a property. Testables are unit, Boolean, Lazy testables, Gen testables,
    /// Async testables, Task testables, and functions from a type for which a generator is known to a testable.
    [<CompiledName("OfTestable")>]
    let ofTestable (testable:'Testable) =
        property testable

    [<CompiledName("Discard")>]
    let discard() = raise DiscardException

///Operators for Prop.
[<AutoOpen>]
module PropOperators =

    open FsCheck.Testable

    ///Conditional property combinator. Resulting property holds if the property after ==> holds whenever the condition does.
    let (==>) condition (assertion:'Testable) = Prop.filter condition assertion

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    let (.&.) (l:'LeftTestable) (r:'RightTestable) = 
        let andProp = l .& r
        andProp

    ///Construct a property that fails if both fail. (cfr 'or')
    let (.|.) (l:'LeftTestable) (r:'RightTestable) = 
        let orProp = l .| r
        orProp
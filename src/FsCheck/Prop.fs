namespace FsCheck

///Combinators to build properties, which define the property to be tested, with some
///convenience methods to investigate the generated arguments and any found counter-examples.
module Prop =
    open Testable
    open System

    ///Quantified property combinator. Provide a custom test data generator to a property.
    let forAll (arb:Arbitrary<'Value>) (body:'Value -> 'Testable) = forAll arb body

    ///Depending on the condition, return the first testable if true and the second if false.
    let given condition (iftrue:'TestableIfTrue, ifFalse:'TestableIfFalse) = 
        if condition then property iftrue else property ifFalse

    ///Expect exception 't when executing p. So, results in success if an exception of the given type is thrown, 
    ///and a failure otherwise.
    let throws<'Exception, 'Testable when 'Exception :> exn> (p : Lazy<'Testable>) = 
       property <| try ignore p.Value; Res.failed with :? 'Exception -> Res.succeeded

    let private stamp str = 
        let add res = { res with Stamp = str :: res.Stamp } 
        Prop.mapResult add

    ///Classify test cases combinator. Test cases satisfying the condition are assigned the classification given.
    let classify b name : ('Testable -> Property) = if b then stamp name else property

    ///Count trivial cases property combinator. Test cases for which the condition is True are classified as trivial.
    let trivial b : ('Testable -> Property) = classify b "trivial"

    ///Collect data values property combinator. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported, using any_to_string.
    let collect (v:'CollectedValue) : ('Testable -> Property) = stamp <| sprintf "%A" v

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    let label l : ('Testable -> Property) = 
        let add res = { res with Labels = Set.add l res.Labels }
        Prop.mapResult add

    ///Fails the property if it does not complete within t milliseconds. Note that the called property gets a
    ///cancel signal, but whether it responds to that is up to the property; the execution may not actually stop.
    let within time (lazyProperty:Lazy<'Testable>) =
        try 
            let test = new Func<_>(fun () -> property lazyProperty.Value)
            let asyncTest = Async.FromBeginEnd(test.BeginInvoke, test.EndInvoke)                     
            Async.RunSynchronously(asyncTest, timeout = time)
        with
            :? TimeoutException -> 
                Async.CancelDefaultToken()
                property (Res.timeout time)

    /// Turns a testable type into a property. Testables are unit, boolean, Lazy testables, Gen testables, functions
    /// from a type for which a generator is know to a testable, tuples up to 6 tuple containing testables, and lists
    /// containing testables.
    let ofTestable (testable:'Testable) =
        property testable


///Operators for Prop.
[<AutoOpen>]
module PropOperators =

    open Testable

    ///Conditional property combinator. Resulting property holds if the property after ==> holds whenever the condition does.
    let (==>) condition (assertion:'Testable) = Prop.given condition (assertion,property Res.rejected)

    ///Add the given label to the property. Property on the left hand side, label on the right.
    let (|@) x y = (Common.flip Prop.label) x y

    ///Add the given label to the property. label on the left hand side, property on the right.
    let (@|) = Prop.label

    ///Add the given label to the property. Property on the left hand side, label on the right.
    let (%>) = (|@)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    let (.&.) (l:'LeftTestable) (r:'RightTestable) = 
        let andProp = l .& r
        andProp

    ///Construct a property that fails if both fail. (cfr 'or')
    let (.|.) (l:'LeftTestable) (r:'RightTestable) = 
        let orProp = l .| r
        orProp


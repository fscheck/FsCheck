namespace FsCheck

///Combinators to build properties, which define the property to be tested, with some
///convenience methods to investigate the generated arguments and any found counter-examples.
module Prop =
    open Testable
    open System
    open System.ComponentModel

    ///Quantified property combinator. Provide a custom test data generator to a property.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let forAll (arb:Arbitrary<'Value>) (body:'Value -> 'Testable) = forAll arb body

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllAction1 (arb:Arbitrary<'Value>) (body:Action<'Value>) = forAll arb body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc1Bool (arb:Arbitrary<'Value>) (body:Func<'Value,bool>) = forAll arb body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc1Prop (arb:Arbitrary<'Value>) (body:Func<'Value,Property>) = forAll arb body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllAction1Def (body:Action<'Value>) = property body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc1BoolDef (body:Func<'Value,bool>) = property body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc1PropDef (body:Func<'Value,Property>) = property body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllAction2 (arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>) (body:Action<'V1,'V2>) = forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> body.Invoke(v1,v2)))

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc2Bool (arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>) (body:Func<'V1,'V2,bool>) = forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> body.Invoke(v1,v2)))

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc2Prop (arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>) (body:Func<'V1,'V2,Property>) = forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> body.Invoke(v1,v2)))

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllAction2Def (body:Action<'V1,'V2>) = property body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc2BoolDef (body:Func<'V1,'V2,bool>) = property body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc2PropDef (body:Func<'V1,'V2,Property>) = property body.Invoke
    
    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllAction3 (arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>,arb3:Arbitrary<'V3>) (body:Action<'V1,'V2,'V3>) = 
        forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> forAll arb3 (fun v3 -> body.Invoke(v1,v2,v3))))

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc3Bool (arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>,arb3:Arbitrary<'V3>) (body:Func<'V1,'V2,'V3,bool>) = 
        forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> forAll arb3 (fun v3 -> body.Invoke(v1,v2,v3))))

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc3Prop (arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>,arb3:Arbitrary<'V3>) (body:Func<'V1,'V2,'V3,Property>) = 
        forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> forAll arb3 (fun v3 -> body.Invoke(v1,v2,v3))))

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllAction3Def (body:Action<'V1,'V2,'V3>) = property body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc3BoolDef (body:Func<'V1,'V2,'V3,bool>) = property body.Invoke

    [<CompiledName("ForAll"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let forAllFunc3PropDef (body:Func<'V1,'V2,'V3,Property>) = property body.Invoke

    ///Depending on the condition, return the first testable if true and the second if false.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let given condition (iftrue:'TestableIfTrue, ifFalse:'TestableIfFalse) = 
        if condition then property iftrue else property ifFalse

    ///Expect exception 't when executing p. So, results in success if an exception of the given type is thrown, 
    ///and a failure otherwise.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let throws<'Exception, 'Testable when 'Exception :> exn> (p : Lazy<'Testable>) = 
       property <| try ignore p.Value; Res.failed with :? 'Exception -> Res.succeeded

    let private stamp str = 
        let add res = { res with Stamp = str :: res.Stamp } 
        Prop.mapResult add

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let classify b name : ('Testable -> Property) = if b then stamp name else property

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let trivial b : ('Testable -> Property) = classify b "trivial"

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported, using sprintf "%A".
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let collect (v:'CollectedValue) : ('Testable -> Property) = stamp <| sprintf "%A" v

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let label l : ('Testable -> Property) = 
        let add res = { res with Labels = Set.add l res.Labels }
        Prop.mapResult add

    ///Fails the property if it does not complete within t milliseconds. Note that the called property gets a
    ///cancel signal, but whether it responds to that is up to the property; the execution may not actually stop.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
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
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let ofTestable (testable:'Testable) =
        property testable

    [<CompiledName("Discard")>]
    let discard() = raise DiscardException

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


namespace FsCheck.Fluent

open System
open System.Runtime.CompilerServices

open FsCheck
open FsCheck.FSharp
open Testable

///Extensions to construct Properties.
[<Extension;Sealed;AbstractClass>]
type Prop private() =

    static member ForAll(arb:Arbitrary<'Value>, body:Action<'Value>) = Prop.forAll arb body.Invoke

    static member ForAll(arb:Arbitrary<'Value>, body:Func<'Value,bool>) = Prop.forAll arb body.Invoke

    static member ForAll(arb:Arbitrary<'Value>, body:Func<'Value,Threading.Tasks.Task>) = forAll arb body.Invoke

    static member ForAll(arb:Arbitrary<'Value>, body:Func<'Value,Threading.Tasks.Task<bool>>) = forAll arb body.Invoke

    static member ForAll(arb:Arbitrary<'Value>, body:Func<'Value,Property>) = forAll arb body.Invoke

    static member ForAll(body:Action<'Value>) = property body.Invoke

    static member ForAll(body:Func<'Value,bool>) = property body.Invoke

    static member ForAll(body:Func<'Value,Threading.Tasks.Task>) = property body.Invoke

    static member ForAll(body:Func<'Value,Threading.Tasks.Task<bool>>) = property body.Invoke

    static member ForAll(body:Func<'Value,Property>) = property body.Invoke

    static member ForAll(arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>, body:Action<'V1,'V2>) = forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> body.Invoke(v1,v2)))

    static member ForAll(arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>, body:Func<'V1,'V2,bool>) = forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> body.Invoke(v1,v2)))

    static member ForAll(arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>, body:Func<'V1,'V2,Property>) = forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> body.Invoke(v1,v2)))

    static member ForAll(body:Action<'V1,'V2>) = property body.Invoke

    static member ForAll(body:Func<'V1,'V2,bool>) = property body.Invoke

    static member ForAll(body:Func<'V1,'V2,Property>) = property body.Invoke
    
    static member ForAll(arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>,arb3:Arbitrary<'V3>, body:Action<'V1,'V2,'V3>) = 
        forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> forAll arb3 (fun v3 -> body.Invoke(v1,v2,v3))))

    static member ForAll(arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>,arb3:Arbitrary<'V3>, body:Func<'V1,'V2,'V3,bool>) = 
        forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> forAll arb3 (fun v3 -> body.Invoke(v1,v2,v3))))

    static member ForAll(arb1:Arbitrary<'V1>,arb2:Arbitrary<'V2>,arb3:Arbitrary<'V3>, body:Func<'V1,'V2,'V3,Property>) = 
        forAll arb1 (fun v1 -> forAll arb2 (fun v2 -> forAll arb3 (fun v3 -> body.Invoke(v1,v2,v3))))

    static member ForAll(body:Action<'V1,'V2,'V3>) = property body.Invoke

    static member ForAll(body:Func<'V1,'V2,'V3,bool>) = property body.Invoke

    static member ForAll(body:Func<'V1,'V2,'V3,Property>) = property body.Invoke

    /// Turns a testable type into a property.
    [<Extension>]
    static member ToProperty (testable:bool) =
        Prop.ofTestable testable

    /// Turns a testable type into a property.
    [<Extension>]
    static member ToProperty (testable:Action) =
        if isNull testable then nullArg "testable"
        Prop.ofTestable testable.Invoke

    /// Turns a testable type into a property.
    [<Extension>]
    static member ToProperty (testable:Func<bool>) =
        if isNull testable then nullArg "testable"
        Prop.ofTestable testable.Invoke
    
    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member When(property:bool, condition) = 
        Prop.given condition (property,Prop.ofTestable Res.rejected)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member When(property:Action, condition) = 
        if isNull property then nullArg "property"
        Prop.given condition (Prop.ToProperty property, Prop.ofTestable Res.rejected)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member When(property:Func<bool>, condition) = 
        if isNull property then nullArg "property"
        Prop.given condition (Prop.ToProperty property, Prop.ofTestable Res.rejected)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member When(property:Property, condition) = 
        Prop.given condition (property,Prop.ofTestable Res.rejected)

    ///Conditional property combinator. Resulting property holds if the given property holds whenever the condition does. See also operator:  'assertion ==> property'
    [<Extension>]
    static member When(condition, assertion : Func<'Testable>) =
        if isNull assertion then nullArg "assertion"
        Prop.filter condition (fun () -> assertion.Invoke ())

    ///Conditional property combinator. Resulting property holds if the given property holds whenever the condition does. See also operator:  'assertion ==> property'
    static member When(condition, assertion : Action) =
        if isNull assertion then nullArg "assertion"
        Prop.filter condition (fun () -> assertion.Invoke ())

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member Implies(condition: bool, property:bool) = 
        Prop.When(property, condition)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member Implies(condition, property:Action) = 
        if isNull property then nullArg "property"
        Prop.When(property, condition)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member Implies(condition, property:Func<bool>) = 
        if isNull property then nullArg "property"
        Prop.When(property, condition)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member Implies(condition, property:Property) = 
        Prop.When(property, condition)

    //Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: Action, cond:bool, name:string) = 
        if isNull property then nullArg "property"
        Prop.classify cond name (Prop.ToProperty(property))

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: bool, cond:bool, name:string) = 
        Prop.classify cond name (Prop.ToProperty(property))

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: Func<bool>, cond:bool, name:string) = 
        if isNull property then nullArg "property"
        Prop.classify cond name (Prop.ToProperty(property))

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: Property, cond:bool, name:string) = 
        if isNull name then nullArg "name"
        Prop.classify cond name property

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:Action, condition) =
        if isNull property then nullArg "property"
        Prop.trivial condition (Prop.ToProperty(property))

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:bool, condition) =
        Prop.trivial condition (Prop.ToProperty(property))

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:Func<bool>, condition) =
        if isNull property then nullArg "property"
        Prop.trivial condition (Prop.ToProperty(property))

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:Property, condition) =
        Prop.trivial condition property

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: Action, v:'CollectedValue) = 
        if isNull property then nullArg "property"
        Prop.collect v (Prop.ToProperty(property))

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: bool, v:'CollectedValue) = 
        Prop.collect v (Prop.ToProperty(property))

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: Func<bool>, v:'CollectedValue) = 
        if isNull property then nullArg "property"
        Prop.collect v (Prop.ToProperty(property))

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: Property, v:'CollectedValue) = 
        Prop.collect v property

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: Action, label) =
        if isNull property then nullArg "property"
        Prop.label label (Prop.ToProperty(property))

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: bool, label) =
        Prop.label label (Prop.ToProperty(property))

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: Func<bool>, label) =
        if isNull property then nullArg "property"
        Prop.label label (Prop.ToProperty(property))

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: Property, label) =
        Prop.label label property

    static member Discard() = Prop.discard()

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: bool, right:Action) =
        if isNull right then nullArg "right"
        (Prop.ToProperty left) .&. (Prop.ToProperty right)
        
    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: bool, right:bool) =
        (Prop.ToProperty left) .&. (Prop.ToProperty right)


    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: bool, right:Func<bool>) =
        if isNull right then nullArg "right"
        (Prop.ToProperty left) .&. (Prop.ToProperty right)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: bool, right:Property) =
        (Prop.ToProperty left) .&. right

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:Action) =
        if isNull right then nullArg "right"
        left .&. (Prop.ToProperty right)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:bool) =
        left .&. (Prop.ToProperty right)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:Func<bool>) =
        if isNull right then nullArg "right"
        left .&. (Prop.ToProperty right)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:Property) =
        left .&. right

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: bool, right:Action) =
        if isNull right then nullArg "right"
        (Prop.ToProperty left) .|. (Prop.ToProperty right)
        
    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: bool, right:bool) =
        (Prop.ToProperty left) .|. (Prop.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: bool, right:Func<bool>) =
        if isNull right then nullArg "right"
        (Prop.ToProperty left) .|. (Prop.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: bool, right:Property) =
        (Prop.ToProperty left) .|. right

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:Action) =
        if isNull right then nullArg "right"
        left .|. (Prop.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:bool) =
        left .|. (Prop.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:Func<bool>) =
        if isNull right then nullArg "right"
        left .|. (Prop.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:Property) =
        left .|. right

namespace FsCheck

open System
open System.Runtime.CompilerServices

///Extensons to construct Properties.
[<Extension>]
type PropertyExtensions =

    /// Turns a testable type into a property.
    [<Extension>]
    static member ToProperty (testable:bool) =
        Prop.ofTestable testable

    /// Turns a testable type into a property.
    [<Extension>]
    static member ToProperty (testable:Action) =
        Prop.ofTestable testable.Invoke

    /// Turns a testable type into a property.
    [<Extension>]
    static member ToProperty (testable:Func<bool>) =
        Prop.ofTestable testable.Invoke
    
    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member When(property:bool, condition) = 
        Prop.given condition (property,Prop.ofTestable Res.rejected)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member When(property:Action, condition) = 
        Prop.given condition (PropertyExtensions.ToProperty property, Prop.ofTestable Res.rejected)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member When(property:Func<bool>, condition) = 
        Prop.given condition (PropertyExtensions.ToProperty property, Prop.ofTestable Res.rejected)

    ///Conditional property combinator. Resulting property holds if the property holds when the condition does.
    [<Extension>]
    static member Implies(condition, property:Func<bool>) = 
        property.When(condition)

    //Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: Action, cond:bool, name:string) = 
        Prop.classify cond name (PropertyExtensions.ToProperty(property))

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: bool, cond:bool, name:string) = 
        Prop.classify cond name (PropertyExtensions.ToProperty(property))

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: Func<bool>, cond:bool, name:string) = 
        Prop.classify cond name (PropertyExtensions.ToProperty(property))

    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<Extension>]
    static member Classify (property: Property, cond:bool, name:string) = 
        Prop.classify cond name property

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:Action, condition) =
        Prop.trivial condition (PropertyExtensions.ToProperty(property))

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:bool, condition) =
        Prop.trivial condition (PropertyExtensions.ToProperty(property))

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:Func<bool>, condition) =
        Prop.trivial condition (PropertyExtensions.ToProperty(property))

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<Extension>]
    static member Trivial(property:Property, condition) =
        Prop.trivial condition property

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: Action, v:'CollectedValue) = 
        Prop.collect v (PropertyExtensions.ToProperty(property))

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: bool, v:'CollectedValue) = 
        Prop.collect v (PropertyExtensions.ToProperty(property))

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: Func<bool>, v:'CollectedValue) = 
        Prop.collect v (PropertyExtensions.ToProperty(property))

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<Extension>]
    static member Collect (property: Property, v:'CollectedValue) = 
        Prop.collect v property

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: Action, label) =
        Prop.label label (PropertyExtensions.ToProperty(property))

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: bool, label) =
        Prop.label label (PropertyExtensions.ToProperty(property))

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: Func<bool>, label) =
        Prop.label label (PropertyExtensions.ToProperty(property))

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<Extension>]
    static member Label(property: Property, label) =
        Prop.label label property

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:Action) =
        left .&. (PropertyExtensions.ToProperty right)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:bool) =
        left .&. (PropertyExtensions.ToProperty right)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:Func<bool>) =
        left .&. (PropertyExtensions.ToProperty right)

    ///Construct a property that succeeds if both succeed. (cfr 'and')
    [<Extension>]
    static member And(left: Property, right:Property) =
        left .&. right

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:Action) =
        left .|. (PropertyExtensions.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:bool) =
        left .|. (PropertyExtensions.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:Func<bool>) =
        left .|. (PropertyExtensions.ToProperty right)

    ///Construct a property that fails if both fail. (cfr 'or')
    [<Extension>]
    static member Or(left: Property, right:Property) =
        left .|. right
        


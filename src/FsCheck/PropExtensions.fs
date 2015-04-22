namespace FsCheck

open System

///Extensons to construct Properties.
[<System.Runtime.CompilerServices.Extension>]
type PropertyExtensions =
    
    ///Classify test cases. Test cases satisfying the condition are assigned the classification given.
    [<System.Runtime.CompilerServices.Extension>]
    static member Classify (property: Property, cond:bool, name:string) = 
        Prop.classify cond name property

    ///Count trivial cases. Test cases for which the condition is True are classified as trivial.
    [<System.Runtime.CompilerServices.Extension>]
    static member Trivial(property:Property, b) =
        Prop.trivial b property

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    [<System.Runtime.CompilerServices.Extension>]
    static member Collect (property: Property, v:'CollectedValue) = 
        Prop.collect v property

    ///Add the given label to the property. The labels of a failing sub-property are displayed when it fails.
    [<System.Runtime.CompilerServices.Extension>]
    static member Label(property: Property, l) =
        Prop.label l property

    /// Turns a testable type into a property.
    [<System.Runtime.CompilerServices.Extension>]
    static member ToProperty (testable:bool) =
        Prop.ofTestable testable

    /// Turns a testable type into a property.
    [<System.Runtime.CompilerServices.Extension>]
    static member ToProperty (testable:Action) =
        Prop.ofTestable testable.Invoke

    /// Turns a testable type into a property.
    [<System.Runtime.CompilerServices.Extension>]
    static member ToProperty (testable:Func<bool>) =
        Prop.ofTestable testable.Invoke

//    /// Turns a testable type into a property.
//    [<System.Runtime.CompilerServices.Extension>]
//    static member ToProperty (testable:Action<'T1>) =
//        Prop.ofTestable testable.Invoke
//
//    /// Turns a testable type into a property.
//    [<System.Runtime.CompilerServices.Extension>]
//    static member ToProperty (testable:Func<'T1,bool>) =
//        Prop.ofTestable testable.Invoke






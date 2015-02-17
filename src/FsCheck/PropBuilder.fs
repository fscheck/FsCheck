(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **  
**  All rights reserved.                                                    **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

open System
open System.ComponentModel
open Prop
open Testable

//TODO:
//Within -> rely on testing frameworks?
//Throws -> rely on testing frameworks?
//"And" and "Or" should start a new property, with own classifies and labels etc (see prop_Label)
//label: maybe add some overloads, should be able to nest (see propMul)


///Specify a property to test.
[<AbstractClass>]
type Specification() =
    inherit obj()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override __.Equals(other) = base.Equals(other)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override __.GetHashCode() = base.GetHashCode()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override __.ToString() = base.ToString()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    abstract Build : unit -> Property



and Specification<'a> internal   ( generator0:'a Gen
                               , shrinker0: 'a -> 'a seq
                               , assertion0:'a -> Property
                               , conditions:('a -> bool) list
                               , collects:('a -> string) list
                               , classifies:(('a -> bool) * string) list) =
    inherit Specification()
    override __.Build() =
            let conditions' a = conditions |> List.fold (fun s f -> s && f a) true
            let collects' a prop = collects |> List.fold (fun prop f -> prop |> collect (f a)) prop
            let classifies' a prop = classifies |> List.fold (fun prop (f,name) -> prop |> classify (f a) name) prop  
            forAll (Arb.fromGenShrink(generator0,shrinker0)) (fun a -> (conditions' a) ==> lazy (assertion0 a) |> collects' a |> classifies' a)
    member __.When( condition:Func<'a,bool> ) = 
        Specification<'a>(generator0, shrinker0, assertion0, (fun a -> condition.Invoke(a))::conditions, collects, classifies)

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    member __.Collect(collectedValue:Func<'a,string>)=
        Specification<'a>(generator0, shrinker0,assertion0,conditions,(fun a -> collectedValue.Invoke(a))::collects,classifies)

    member __.Classify(filter:Func<'a,bool>,name:string) =
        Specification<'a>(generator0, shrinker0,assertion0,conditions,collects,((fun a -> filter.Invoke(a)),name)::classifies)
    member __.Shrink(shrinker:Func<'a,'a seq>) =
        Specification<'a>( generator0, shrinker.Invoke, assertion0, conditions, collects, classifies)
    member __.Label( name:string ) =
        Specification<'a>(generator0, shrinker0, label name << assertion0,conditions, collects, classifies)

    ///Construct a property that succeeds if both succeed.
    member __.And(assertion : Func<'a,bool>) =
        Specification<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .&. (assertion.Invoke(a))), conditions, collects, classifies)

    ///Construct a property that succeeds if both succeed.
    member __.And(assertion : Func<'a,bool>, name:string ) =
        Specification<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .&. (label name (assertion.Invoke(a)))), conditions, collects, classifies)

    ///Construct a property that fails if both fail.
    member __.Or(assertion : Func<'a,bool>) =
        Specification<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .|. (assertion.Invoke(a))), conditions, collects, classifies)

    ///Construct a property that fails if both fail.
    member __.Or(assertion : Func<'a,bool>, name:string ) =
        Specification<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .|. (label name (assertion.Invoke(a)))), conditions, collects, classifies)
    member __.AndFor<'b>(generator:'b Gen, assertion:Func<'b,bool>) =
        Specification<'a,'b>  (generator0
                            ,shrinker0 
                            ,generator
                            ,fun _ -> Seq.empty
                            ,fun a b -> (assertion0 a) .&. property (assertion.Invoke(b))
                            ,conditions |> List.map (fun f -> (fun a _ -> f a))
                            ,collects |> List.map (fun f -> (fun a _ -> f a))
                            ,classifies |> List.map (fun (f,name) -> ((fun a _ -> f a),name))
                            )
  
       
and Specification<'a,'b> internal   ( generator0:'a Gen
                                  , shrinker0: 'a -> 'a seq
                                  , generator1:'b Gen
                                  , shrinker1: 'b -> 'b seq
                                  , assertion0:'a -> 'b -> Property
                                  , conditions:('a -> 'b -> bool) list
                                  , collects:('a -> 'b -> string) list
                                  , classifies:(('a -> 'b -> bool) * string) list) = 
    inherit Specification()
    override __.Build() =
            let conditions' a b = conditions |> List.fold (fun s f -> s && f a b) true
            let collects' a b prop = collects |> List.fold (fun prop f -> prop |> collect (f a b)) prop
            let classifies' a b prop = classifies |> List.fold (fun prop (f,name) -> prop |> classify (f a b) name) prop  
            forAll (Arb.fromGen generator0) (fun a -> forAll (Arb.fromGen generator1) (fun b -> (conditions' a b) ==> lazy (assertion0 a b) |> collects' a b |> classifies' a b))
    member __.When( condition:Func<'a,'b,bool> ) = 
        Specification<'a,'b>(generator0, shrinker0, generator1, shrinker1, assertion0, (fun a b -> condition.Invoke(a,b))::conditions, collects, classifies)
    member __.Collect(collectedValue:Func<'a,'b,string>)=
        Specification<'a,'b>(generator0, shrinker0, generator1, shrinker1, assertion0,conditions,(fun a b -> collectedValue.Invoke(a,b))::collects,classifies)
    member __.Classify(filter:Func<'a,'b,bool>,name:string) =
        Specification<'a,'b>(generator0, shrinker0,generator1, shrinker1,assertion0,conditions,collects,((fun a b -> filter.Invoke(a,b)),name)::classifies)
    member __.Shrink(shrinker:Func<'b,'b seq>) =
        Specification<'a,'b>( generator0, shrinker0, generator1, shrinker.Invoke, assertion0, conditions, collects, classifies)
    member __.Label( name:string ) =
        Specification<'a,'b>(generator0, shrinker0, generator1, shrinker1, (fun a b-> label name (assertion0 a b)),conditions, collects, classifies)
    member __.And(assertion : Func<'a,'b,bool>) =
        Specification<'a,'b>( generator0, shrinker0, generator1, shrinker1,
            (fun a b -> (assertion0 a b) .&. (assertion.Invoke(a, b))) , conditions, collects, classifies)
    member __.And(assertion : Func<'a,'b,bool>, name:string ) =
        Specification<'a,'b>( generator0, shrinker0, generator1, shrinker1, 
            (fun a b -> (assertion0 a b) .&. (label name (assertion.Invoke(a,b)))), conditions, collects, classifies)
    member __.Or(assertion : Func<'a,'b,bool>) =
        Specification<'a,'b>( generator0, shrinker0, generator1, shrinker1, 
            (fun a b -> (assertion0 a b) .|. (assertion.Invoke(a,b))), conditions, collects, classifies)
    member __.Or(assertion : Func<'a,'b,bool>, name:string ) =
        Specification<'a,'b>( generator0, shrinker0, generator1, shrinker1, 
            (fun a b -> (assertion0 a b) .|. (label name (assertion.Invoke(a,b)))), conditions, collects, classifies)
    member __.AndFor<'c>(generator:'c Gen, assertion:Func<'c,bool>) =
        Specification<'a,'b,'c>   (generator0, shrinker0
                                ,generator1, shrinker1
                                ,generator, fun _ -> Seq.empty
                                ,fun a b c -> (assertion0 a b) .&. property (assertion.Invoke(c))
                                ,conditions |> List.map (fun f -> (fun a b _ -> f a b))
                                ,collects |> List.map (fun f -> (fun a b _ -> f a b))
                                ,classifies |> List.map (fun (f,name) -> (fun a b _ -> f a b),name)
                                )
                                
and Specification<'a,'b,'c> internal  ( generator0:'a Gen
                                    , shrinker0:'a -> 'a seq
                                    , generator1:'b Gen
                                    , shrinker1: 'b -> 'b seq
                                    , generator2:'c Gen
                                    , shrinker2: 'c -> 'c seq
                                    , assertion0:'a -> 'b -> 'c -> Property
                                    , conditions:('a -> 'b -> 'c -> bool) list
                                    , collects:('a -> 'b -> 'c -> string) list
                                    , classifies:(('a -> 'b -> 'c -> bool) * string) list) = 
    inherit Specification()
    override __.Build() =
            let conditions' a b c = conditions |> List.fold (fun s f -> s && f a b c) true
            let collects' a b c prop = collects |> List.fold (fun prop f -> prop |> collect (f a b c)) prop
            let classifies' a b c prop = classifies |> List.fold (fun prop (f,name) -> prop |> classify (f a b c) name) prop  
            forAll (Arb.fromGen generator0) (fun a -> 
            forAll (Arb.fromGen generator1) (fun b -> 
            forAll (Arb.fromGen generator2) (fun c ->
                (conditions' a b c) ==> lazy (assertion0 a b c) |> collects' a b c |> classifies' a b c))) 
    member __.When( condition:Func<'a,'b,'c,bool> ) = 
        Specification<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, assertion0, (fun a b c -> condition.Invoke(a,b,c))::conditions, collects, classifies)
    member __.Collect(collectedValue:Func<'a,'b,'c,string>)=
        Specification<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, assertion0, conditions,(fun a b c -> collectedValue.Invoke(a,b,c))::collects,classifies)
    member __.Classify(filter:Func<'a,'b,'c,bool>,name:string) =
        Specification<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, assertion0, conditions, collects,((fun a b c -> filter.Invoke(a,b,c)),name)::classifies)         
    member __.Shrink(shrinker:Func<'c,'c seq>) =
        Specification<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker.Invoke, assertion0, conditions, collects, classifies)
    member __.Label( name:string ) =
        Specification<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, (fun a b c -> label name (assertion0 a b c)),conditions, collects, classifies)
    member __.And(assertion : Func<'a,'b,'c,bool>) =
        Specification<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1,generator2, shrinker2,
            (fun a b c -> (assertion0 a b c) .&. (assertion.Invoke(a, b, c))) , conditions, collects, classifies)
    member __.And(assertion : Func<'a,'b,'c,bool>, name:string ) =
        Specification<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1, generator2, shrinker2,
            (fun a b c -> (assertion0 a b c) .&. (label name (assertion.Invoke(a,b,c)))), conditions, collects, classifies)
    member __.Or(assertion : Func<'a,'b,'c,bool>) =
        Specification<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1, generator2, shrinker2,
            (fun a b c -> (assertion0 a b c) .|. (assertion.Invoke(a,b,c))), conditions, collects, classifies)
    member __.Or(assertion : Func<'a,'b,'c,bool>, name:string ) =
        Specification<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1,generator2, shrinker2, 
            (fun a b c -> (assertion0 a b c) .|. (label name (assertion.Invoke(a,b,c)))), conditions, collects, classifies)  
      
///Entry point to specifying a property.
[<AbstractClass;Sealed>]
type Prop private() =
    static do ignore Arb.init.Value
    static let noshrink = fun _ -> Seq.empty

    static member ForAll(assertion:bool) =
        Specification<unit>(Arb.from.Generator, Arb.from.Shrinker, (fun () -> property assertion), [], [], [])

    static member ForAll(assertion:Action) =
        Specification<unit>(Arb.from.Generator, Arb.from.Shrinker, property << assertion.Invoke, [], [], [])
    static member ForAll(assertion:Func<bool>) =
        Specification<unit>(Arb.from.Generator, Arb.from.Shrinker, property << assertion.Invoke, [], [], [])

    static member ForAll(assertion:Action<'a>) =
        Prop.ForAll(Arb.from, assertion)
    static member ForAll(assertion:Func<'a,bool>) =
        Prop.ForAll(Arb.from, assertion)
    
    static member ForAll(assertion:Action<'a,'b>) =
        Prop.ForAll(Arb.from, Arb.from, assertion)
    static member ForAll(assertion:Func<'a,'b,bool>) =
        Prop.ForAll(Arb.from, Arb.from, assertion)

    static member ForAll(assertion:Action<'a,'b,'c>) =
        Prop.ForAll(Arb.from, Arb.from, Arb.from, assertion)
    static member ForAll(assertion:Func<'a,'b,'c,bool>) =
        Prop.ForAll(Arb.from, Arb.from, Arb.from, assertion)
     
    static member ForAll(generator:'a Gen, assertion:Func<'a,bool>) =
        Specification<'a>(generator, noshrink, property << assertion.Invoke, [], [], [])
    static member ForAll(arbitrary:'a Arbitrary, assertion:Func<'a,bool>) =
        Specification<'a>(arbitrary.Generator, arbitrary.Shrinker, property << assertion.Invoke, [], [], [])

    static member ForAll(generator:'a Gen, assertion:Action<'a>) =
        Specification<'a>(generator, noshrink, property << assertion.Invoke, [], [], [])
    static member ForAll(arbitrary:'a Arbitrary, assertion:Action<'a>) =
        Specification<'a>(arbitrary.Generator, arbitrary.Shrinker, property << assertion.Invoke, [], [], [])

    static member ForAll(generator1:'a Gen,generator2:'b Gen, assertion:Func<'a,'b,bool>) =
        Specification<'a,'b>(generator1, noshrink, generator2, noshrink, (fun a b -> property <| assertion.Invoke(a,b)),[],[],[])
    static member ForAll(arbitrary1:'a Arbitrary,arbitrary2:'b Arbitrary, assertion:Func<'a,'b,bool>) =
        Specification<'a,'b>(arbitrary1.Generator, arbitrary1.Shrinker, arbitrary2.Generator, arbitrary2.Shrinker, (fun a b -> property <| assertion.Invoke(a,b)),[],[],[])

    static member ForAll(generator1:'a Gen,generator2:'b Gen, assertion:Action<'a,'b>) =
        Specification<'a,'b>(generator1, noshrink, generator2, noshrink, (fun a b -> property <| assertion.Invoke(a,b)),[],[],[])
    static member ForAll(arbitrary1:'a Arbitrary,arbitrary2:'b Arbitrary, assertion:Action<'a,'b>) =
        Specification<'a,'b>(arbitrary1.Generator, arbitrary1.Shrinker, arbitrary2.Generator, arbitrary2.Shrinker, (fun a b -> property <| assertion.Invoke(a,b)),[],[],[])

    static member ForAll(generator1:'a Gen,generator2:'b Gen,generator3:'c Gen, assertion:Func<'a,'b,'c,bool>) =
        Specification<'a,'b,'c>(generator1, noshrink, generator2, noshrink, generator3, noshrink, (fun a b c -> property <| assertion.Invoke(a,b,c)),[],[],[])
    static member ForAll(arbitrary1:'a Arbitrary,arbitrary2:'b Arbitrary,arbitrary3:'c Arbitrary, assertion:Func<'a,'b,'c,bool>) =
        Specification<'a,'b,'c>(arbitrary1.Generator, arbitrary1.Shrinker, arbitrary2.Generator, arbitrary2.Shrinker, arbitrary3.Generator, arbitrary3.Shrinker, (fun a b c -> property <| assertion.Invoke(a,b,c)),[],[],[])

    static member ForAll(generator1:'a Gen,generator2:'b Gen,generator3:'c Gen, assertion:Action<'a,'b,'c>) =
        Specification<'a,'b,'c>(generator1, noshrink, generator2, noshrink, generator3, noshrink, (fun a b c -> property <| assertion.Invoke(a,b,c)),[],[],[])
    static member ForAll(arbitrary1:'a Arbitrary,arbitrary2:'b Arbitrary,arbitrary3:'c Arbitrary, assertion:Action<'a,'b,'c>) =
        Specification<'a,'b,'c>(arbitrary1.Generator, arbitrary1.Shrinker, arbitrary2.Generator, arbitrary2.Shrinker, arbitrary3.Generator, arbitrary3.Shrinker, (fun a b c -> property <| assertion.Invoke(a,b,c)),[],[],[])



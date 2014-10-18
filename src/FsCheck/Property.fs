(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2013 Kurt Schelfthout. All rights reserved.          **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

[<NoComparison>]
type Outcome = 
    | Timeout of int
    | Exception of exn
    | False
    | True
    | Rejected 
    /// determines for which Outcome the result should be shrunk, or shrinking should continue.
    member internal x.Shrink = match x with Exception _ -> true | False -> true | _ -> false 


///The result of one execution of a property.
[<NoComparison>]
type Result = 
    {   Outcome     : Outcome
        Stamp       : list<string>
        Labels      : Set<string>
        Arguments   : list<obj> } 
    ///Returns a new result that is Succeeded if and only if both this
    ///and the given Result are Succeeded.
    static member (&&&) (l,r) = 
        //printfn "And of l %A and r %A" l.Outcome r.Outcome
        match (l.Outcome,r.Outcome) with
        | (Exception _,_) -> l //here a potential exception in r is thrown away...
        | (_,Exception _) -> r
        | (Timeout _,_) -> l
        | (_,Timeout _) -> r
        | (False,_) -> l
        | (_,False) -> r
        | (_,True) -> l
        | (True,_) -> r
        | (Rejected,Rejected) -> l //or r, whatever
    static member (|||) (l,r) =
        match (l.Outcome, r.Outcome) with
        | (Exception _,_) -> l //here a potential exception in r is thrown away...
        | (_,Exception _) -> r
        | (Timeout _,_) -> l
        | (_,Timeout _) -> r
        | (_,False) -> l
        | (False,_) -> r
        | (True,_) -> l
        | (_,True) -> r
        | (Rejected,Rejected) -> l //or r, whatever  

module internal Res =

    let private result =
      { Outcome     = Rejected
        Stamp       = []
        Labels       = Set.empty
        Arguments   = []
      }

    let failed = { result with Outcome = False }

    let exc e = { result with Outcome = Exception e }

    let timeout i = { result with Outcome = Timeout i }

    let succeeded = { result with Outcome = True }

    let rejected = { result with Outcome = Rejected }

     


//A rose is a pretty tree
//Draw it and you'll see.
//A Rose<Result> is used to keep, in a lazy way, a Result and the possible shrinks for the value in the node.
//The fst value is the current Result, and the list contains the properties yielding possibly shrunk results.
//Each of those can in turn have their own shrinks. 
[<NoEquality;NoComparison>]
type Rose<'a> = internal MkRose of Lazy<'a> * seq<Rose<'a>>

module internal Rose =

    let rec map f (a:Rose<_>) = match a with MkRose (x,rs) -> MkRose (lazy (f x.Value), rs |> Seq.map (map f))
       
    //careful here: we can't pattern match as follows, as this will result in evaluation:
    //let rec join (MkRose (Lazy (MkRose(x,ts)),tts)) 
    //instead, define everything inside the MkRose, as a lazily evaluated expression. Haskell would use an irrefutable
    //pattern here (is this possible using an active pattern? -> to investigate!) 
    let rec join (MkRose (r,tts)) =
        //bweurgh. Need to match twice to keep it lazy.
        let x = lazy (match r with (Lazy (MkRose (x,_))) -> x.Value)
        let ts = Seq.append (Seq.map join tts) (match r with (Lazy (MkRose (_,ts))) -> ts)
        MkRose (x,ts) 
      //first shrinks outer quantification; makes most sense
      //first shrinks inner quantification: MkRose (x,(ts ++ Seq.map join tts))

    let ret x = MkRose (lazy x,Seq.empty)
    
    let bind m k = join ( map k m ) 

    let map2 f r1 r2 =  bind r1 (fun r1' -> bind r2 (fun r2' -> ret <| f r1' r2'))
                                     
    let ofLazy x = MkRose (x,Seq.empty)

    type Builder() =
        member internal b.Return(x) : Rose<_> = ret x
        member internal b.Bind(m, k) : Rose<_> = bind m k
                         
    let rose = new Builder()


///Type synonym for a test result generator.
type Property = Gen<Rose<Result>>

module internal Testable =

    open System
    open System.Reflection // for Testable<MethodInfo>
    open Common
    open TypeClass
                   
    type Testable<'a> =
        abstract Property : 'a -> Property
    
    type Testables = class end
     
    let internal TestableTC = 
        (lazy
            let empty = TypeClass<Testable<obj>>.New()
            empty.DiscoverAndMerge(onlyPublic=false,instancesType=typeof<Testables>)).Force()
        
    let property<'a> p = TestableTC.InstanceFor<'a,Testable<'a>>().Property p

    module internal Prop = 
    
        let ofRoseResult t : Property = gen { return t }

        let ofResult (r:Result) : Property = 
            ofRoseResult <| Rose.rose { return r }
         
        let ofBool b = ofResult <| if b then Res.succeeded else Res.failed

        let mapRoseResult f :( _ -> Property) = Gen.map f << property

        let mapResult f = mapRoseResult (Rose.map f)

        let safeForce (body:Lazy<_>) =
            try
                property body.Value
            with
                e -> ofResult (Res.exc e)
    

    let shrinking shrink x pf : Property =
        let promoteRose m = Gen (fun s r -> Rose.map (fun (Gen m') -> m' s r) m)
        //cache is important here to avoid re-evaluation of property
        let rec props x = MkRose (lazy (property (pf x)), shrink x |> Seq.map props |> Seq.cache)
        Gen.map Rose.join <| promoteRose (props x)
     
    let private evaluate body a : Property =
        let argument a res = { res with Arguments = (box a) :: res.Arguments }
        //safeForce (lazy ( body a )) //this doesn't work - exception escapes??
        try 
            body a |> property
        with e -> 
            Prop.ofResult (Res.exc e)
        |> Gen.map (Rose.map (argument a))


    let forAll (arb:Arbitrary<_>) body : Property =
        gen{let! a = arb.Generator
            return! shrinking arb.Shrinker a (fun a' -> evaluate body a')}

    type Testables with
        static member Unit() =
            { new Testable<unit> with
                member x.Property _ = Prop.ofResult Res.succeeded }
        static member Bool() =
            { new Testable<bool> with
                member x.Property b = Prop.ofBool b }
        static member Lazy() =
            { new Testable<Lazy<'a>> with
                member x.Property b =
                    let promoteLazy (m:Lazy<_>) = 
                        Gen (fun s r -> Rose.join <| Rose.ofLazy (lazy (match m.Value with (Gen g) -> g s r)))
                    promoteLazy (lazy (Prop.safeForce (lazy b.Value))) } //TODO: check if the lazy b.Value is necessary (b is already lazy?)
        static member Result() =
            { new Testable<Result> with
                member x.Property res = Prop.ofResult res }
        static member Property() =
            { new Testable<Property> with
                member x.Property prop = prop }
        static member Gen() =
            { new Testable<Gen<'a>> with
                member x.Property gena = gen { let! a = gena in return! property a } }
        static member RoseResult() =
            { new Testable<Rose<Result>> with
                member x.Property rosea = gen { return rosea } } 
        static member Arrow() =
            { new Testable<('a->'b)> with
                member x.Property f = forAll Arb.from f }
//        static member MethodInfo() =
//            { new Testable<MethodInfo> with
//                member x.Property m =
//                    let fromTypes = m.GetParameters() |> Array.map (fun p -> p.ParameterType) 
////                    let fromP = fromTypes |> arrayToTupleType
////                    //we can check methods that return void or unit. We cannot add it to  to the Testable typeclass
////                    //since F# won't let us define that - System.Void can only be used in typeof<> expressions.
////                    //hence the translation here.
////                    let toP = if m.ReturnType = typeof<System.Void> then typeof<unit> else m.ReturnType
////                    let funType = FSharpType.MakeFunctionType(fromP, toP)
//                    TestableTC.GetInstance m.ReturnType
//                    let invokeAndThrowInner (m:MethodInfo) o = 
//                        try
//                            Reflect.invokeMethod m None o
//                         with :? TargetInvocationException as e -> //this is just to avoid huge non-interesting stacktraces in the output
//                            raise (Reflect.preserveStackTrace  e.InnerException)
//                    let funValue = Microsoft.FSharp.Reflection.FSharpValue.MakeFunction(funType, (tupleToArray fromTypes) >> invokeAndThrowInner m)
//                    let genericM = checkMethodInfo.MakeGenericMethod([|funType|])
//                    genericM.Invoke(null, [|box config; funValue|]) |> ignore }

    let private combine f a b:Property = 
        let pa = property a
        let pb = property b
        Gen.map2 (Rose.map2 f) pa pb

    let (.&) l r = combine (&&&) l r

    let (.|) l r = combine (|||) l r

    type Testables with
        static member Tuple2() =
            { new Testable<'a*'b> with
                member x.Property ((a,b)) = a .& b }
        static member Tuple3() =
            { new Testable<'a*'b*'c> with
                member x.Property ((a,b,c)) = a .& b .& c }
        static member Tuple4() =
            { new Testable<'a*'b*'c*'d> with
                member x.Property ((a,b,c,d)) = a .& b .& c .& d }
        static member Tuple5() =
            { new Testable<'a*'b*'c*'d*'e> with
                member x.Property ((a,b,c,d,e)) = a .& b .& c .& d .& e }
        static member Tuple6() =
            { new Testable<'a*'b*'c*'d*'e*'f> with
                member x.Property ((a,b,c,d,e,f)) = a .& b .& c .& d .& e .& f}
        static member List() =
            { new Testable<list<'a>> with
                member x.Property l = List.fold (.&) (property <| List.head l) (List.tail l) }


///Combinators to build properties, which define the property to be tested, with some
///convenience methods to investigate the generated arguments and any found counter-examples.
module Prop =
    open Testable
    open System

    ///Quantified property combinator. Provide a custom test data generator to a property.
    let forAll (arb:Arbitrary<'Value>) (body:'Value -> 'Testable) = forAll arb body

    ///Depending on the condition, return the first testable if true and the second if false.
    let given condition (iftrue:'TestableIfTrue,ifFalse:'TestableIfFalse) = 
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
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

[<NoComparison; RequireQualifiedAccess>]
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
        | (Outcome.Exception _,_) -> l //here a potential exception in r is thrown away...
        | (_,Outcome.Exception _) -> r
        | (Outcome.Timeout _,_) -> l
        | (_,Outcome.Timeout _) -> r
        | (Outcome.False,_) -> l
        | (_,Outcome.False) -> r
        | (_,Outcome.True) -> l
        | (Outcome.True,_) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever
    static member (|||) (l,r) =
        match (l.Outcome, r.Outcome) with
        | (Outcome.Exception _,_) -> l //here a potential exception in r is thrown away...
        | (_,Outcome.Exception _) -> r
        | (Outcome.Timeout _,_) -> l
        | (_,Outcome.Timeout _) -> r
        | (_,Outcome.False) -> l
        | (Outcome.False,_) -> r
        | (Outcome.True,_) -> l
        | (_,Outcome.True) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever  

module internal Res =

    let private result =
      { Outcome     = Outcome.Rejected
        Stamp       = []
        Labels       = Set.empty
        Arguments   = []
      }

    let failed = { result with Outcome = Outcome.False }

    let exc e = { result with Outcome = Outcome.Exception e }

    let timeout i = { result with Outcome = Outcome.Timeout i }

    let succeeded = { result with Outcome = Outcome.True }

    let rejected = { result with Outcome = Outcome.Rejected }

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


///Type synonym for a test result generator.
type Property = private Property of Gen<Rose<Result>> with static member internal GetGen (Property g) = g

module private Testable =

    exception DiscardException

    open System
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
    
        let ofRoseResult t : Property = Property <| gen { return t }

        let ofResult (r:Result) : Property = 
            ofRoseResult <| Rose.ret r
         
        let ofBool b = ofResult <| if b then Res.succeeded else Res.failed

        let mapRoseResult f a = property a |> Property.GetGen |> Gen.map f |> Property

        let mapResult f = mapRoseResult (Rose.map f)

        let safeForce (body:Lazy<_>) =
            try
                property body.Value
            with
            | :? DiscardException -> ofResult Res.rejected
            | e -> ofResult (Res.exc e)    

    let private shrinking shrink x pf =
        let promoteRose m = Gen (fun s r -> Rose.map (fun (Gen m') -> m' s r) m)
        //cache is important here to avoid re-evaluation of property
        let rec props x = MkRose (lazy (property (pf x) |> Property.GetGen), shrink x |> Seq.map props |> Seq.cache)
        promoteRose (props x)
        |> Gen.map Rose.join
     
    let private evaluate body a =
        let argument a res = { res with Arguments = (box a) :: res.Arguments }
        //safeForce (lazy ( body a )) //this doesn't work - exception escapes??
        try 
            body a |> property
        with
        | :? DiscardException -> Prop.ofResult Res.rejected
        | e -> Prop.ofResult (Res.exc e)
        |> Property.GetGen 
        |> Gen.map (Rose.map (argument a))


    let forAll (arb:Arbitrary<_>) body : Property =
        gen{let! a = arb.Generator
            return! shrinking arb.Shrinker a (fun a' -> evaluate body a')}
        |> Property

    let private combine f a b:Property = 
        let pa = property a |> Property.GetGen
        let pb = property b |> Property.GetGen
        Gen.map2 (Rose.map2 f) pa pb
        |> Property

    let (.&) l r = combine (&&&) l r

    let (.|) l r = combine (|||) l r

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
                    promoteLazy (lazy (Prop.safeForce b |> Property.GetGen)) |> Property } 
        static member Result() =
            { new Testable<Result> with
                member x.Property res = Prop.ofResult res }
        static member Property() =
            { new Testable<Property> with
                member x.Property prop = prop }
        static member Gen() =
            { new Testable<Gen<'a>> with
                member x.Property gena = gen { let! a = gena in return! property a |> Property.GetGen } |> Property }
        static member RoseResult() =
            { new Testable<Rose<Result>> with
                member x.Property rosea = gen { return rosea } |> Property }
        static member Arrow() =
            { new Testable<('a->'b)> with
                member x.Property f = forAll Arb.from f }
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

namespace FsCheck

open System

[<NoComparison; RequireQualifiedAccess>]
type Outcome = 
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
    static member resAnd l r = 
        //printfn "And of l %A and r %A" l.Outcome r.Outcome
        match (l.Outcome,r.Outcome) with
        | (Outcome.Exception _,_) -> l //here a potential exception in r is thrown away...
        | (_,Outcome.Exception _) -> r
        | (Outcome.False,_) -> l
        | (_,Outcome.False) -> r
        | (_,Outcome.True) -> l
        | (Outcome.True,_) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever
    static member resOr l r =
        match (l.Outcome, r.Outcome) with
        | (Outcome.Exception _,_) -> r
        | (_,Outcome.Exception _) -> l
        | (_,Outcome.False) -> l
        | (Outcome.False,_) -> r
        | (Outcome.True,_) -> l
        | (_,Outcome.True) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever

type ResultContainer = 
    | Value of Result
    | Future of Threading.Tasks.Task<Result>
    static member (&&&) (l,r) = 
        //printfn "And of l %A and r %A" l.Outcome r.Outcome
        match (l,r) with
        | (Value vl,Value vr) -> Result.resAnd vl vr |> Value
        | (Future tl,Value vr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.resAnd x.Result vr) |> Future
        | (Value vl,Future tr) -> tr.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.resAnd x.Result vl) |> Future
        | (Future tl,Future tr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> 
            tr.ContinueWith (fun (y :Threading.Tasks.Task<Result>) -> Result.resAnd x.Result y.Result)) |> Threading.Tasks.TaskExtensions.Unwrap |> Future
    static member (|||) (l,r) =
        match (l,r) with
        | (Value vl,Value vr) -> Result.resOr vl vr |> Value
        | (Future tl,Value vr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.resOr x.Result vr) |> Future
        | (Value vl,Future tr) -> tr.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.resOr x.Result vl) |> Future
        | (Future tl,Future tr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> 
            tr.ContinueWith (fun (y :Threading.Tasks.Task<Result>) -> Result.resOr x.Result y.Result)) |> Threading.Tasks.TaskExtensions.Unwrap |> Future

module internal Res =

    let private result =
      { Outcome     = Outcome.Rejected
        Stamp       = []
        Labels      = Set.empty
        Arguments   = []
      }

    let failed = { result with Outcome = Outcome.False } |> Value

    let exc e = { result with Outcome = Outcome.Exception e } |> Value

    let succeeded = { result with Outcome = Outcome.True } |> Value

    let rejected = { result with Outcome = Outcome.Rejected } |> Value

    let rejectedV = { result with Outcome = Outcome.Rejected }

    let future (t :Threading.Tasks.Task<Outcome>) = t.ContinueWith (fun (x :Threading.Tasks.Task<Outcome>) -> { result with Outcome = x.Result }) |> Future

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


///A Property can be checked by FsCheck.
type Property = private Property of Gen<Rose<ResultContainer>> with static member internal GetGen (Property g) = g

module private Testable =

    exception DiscardException

    open System
    open TypeClass
                   
    type Testable<'a> =
        abstract Property : 'a -> Property
    
    type Testables = class end
     
    let internal testableTC = 
        (lazy
            let empty = TypeClass<Testable<obj>>.New()
            empty.DiscoverAndMerge(onlyPublic=false,instancesType=typeof<Testables>)).Force()
        
    let property<'a> p = testableTC.InstanceFor<'a,Testable<'a>>().Property p

    module internal Prop = 
    
        let ofRoseResult t : Property = Property <| gen { return t }

        let ofResult (r:ResultContainer) : Property = 
            ofRoseResult <| Rose.ret r
         
        let ofBool b = ofResult <| if b then Res.succeeded else Res.failed
        
        let ofTaskBool (b :Threading.Tasks.Task<bool>) = 
            ofResult <| Res.future (b.ContinueWith (fun (x :Threading.Tasks.Task<bool>) -> 
                if x.IsCompleted then
                    if x.Result then Outcome.True else Outcome.False
                else Outcome.Exception x.Exception))
        
        let ofTask (b :Threading.Tasks.Task) = 
            ofResult <| Res.future (b.ContinueWith (fun (x :Threading.Tasks.Task) -> 
                if x.IsCompleted then Outcome.True else Outcome.Exception x.Exception))

        let mapRoseResult f a = property a |> Property.GetGen |> Gen.map f |> Property

        let mapResult f = mapRoseResult (Rose.map f)

        let safeForce (body:Lazy<_>) =
            try
                property body.Value
            with
            | :? DiscardException -> ofResult Res.rejected
            | e -> ofResult (Res.exc e)

    let private shrinking shrink x pf =
        let promoteRose (m:Rose<Gen<_>>) : Gen<Rose<_>> = 
            Gen (fun s r -> let mr = Rose.map (fun (Gen m') -> (m' s r).Value) m in GeneratedValue (mr,r))
        //cache is important here to avoid re-evaluation of property
        let rec props x = MkRose (lazy (property (pf x) |> Property.GetGen), shrink x |> Seq.map props |> Seq.cache)
        promoteRose (props x)
        |> Gen.map Rose.join
    
    let private evaluate body a =
        let argument a res = 
            match res with
            | ResultContainer.Value r -> { r with Arguments = (box a) :: r.Arguments } |> Value
            | ResultContainer.Future t -> t.ContinueWith (fun (rt :Threading.Tasks.Task<Result>) -> 
                let r = rt.Result
                { r with Arguments = (box a) :: r.Arguments }) |> Future
        //safeForce (lazy ( body a )) //this doesn't work - exception escapes??
        try 
            body a |> property
        with
            | :? DiscardException -> Prop.ofResult Res.rejected
            | e -> Prop.ofResult (Res.exc e)
        |> Property.GetGen 
        |> Gen.map (Rose.map (argument a))


    let forAll (arb:Arbitrary<_>) body : Property =
        let generator = arb.Generator
        let shrinker = arb.Shrinker
        gen { let! a = generator
              return! shrinking shrinker a (fun a' -> evaluate body a')}
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
                member __.Property _ = Prop.ofResult Res.succeeded }
        static member Bool() =
            { new Testable<bool> with
                member __.Property b = Prop.ofBool b }
        static member TaskBool() =
            { new Testable<Threading.Tasks.Task<bool>> with
                member __.Property b = Prop.ofTaskBool b }
        static member Task() =
            { new Testable<Threading.Tasks.Task> with
                member __.Property b = Prop.ofTask b }
        static member AsyncBool() =
            { new Testable<Async<bool>> with
                member __.Property b = Prop.ofTaskBool <| Async.StartAsTask b }
        static member Async() =
            { new Testable<Async<unit>> with
                member __.Property b = Prop.ofTask <| Async.StartAsTask b }
        static member Lazy() =
            { new Testable<Lazy<'a>> with
                member __.Property b =
                    let promoteLazy (m:Lazy<_>) = 
                        Gen (fun s r -> GeneratedValue ((Rose.join <| Rose.ofLazy (lazy (match m.Value with (Gen g) -> (g s r).Value))), r))
                    promoteLazy (lazy (Prop.safeForce b |> Property.GetGen)) |> Property } 
        static member Result() =
            { new Testable<Result> with
                member __.Property res = Prop.ofResult <| Value res }
        static member ResultContainer() =
            { new Testable<ResultContainer> with
                member __.Property resC = Prop.ofResult resC }
        static member Property() =
            { new Testable<Property> with
                member __.Property prop = prop }
        static member Gen() =
            { new Testable<Gen<'a>> with
                member __.Property gena = gen { let! a = gena in return! property a |> Property.GetGen } |> Property }
        static member RoseResult() =
            { new Testable<Rose<ResultContainer>> with
                member __.Property rosea = gen { return rosea } |> Property }
        static member Arrow() =
            { new Testable<('a->'b)> with
                member __.Property f = forAll Arb.from f }
        static member Tuple2() =
            { new Testable<'a*'b> with
                member __.Property ((a,b)) = a .& b }
        static member Tuple3() =
            { new Testable<'a*'b*'c> with
                member __.Property ((a,b,c)) = a .& b .& c }
        static member Tuple4() =
            { new Testable<'a*'b*'c*'d> with
                member __.Property ((a,b,c,d)) = a .& b .& c .& d }
        static member Tuple5() =
            { new Testable<'a*'b*'c*'d*'e> with
                member __.Property ((a,b,c,d,e)) = a .& b .& c .& d .& e }
        static member Tuple6() =
            { new Testable<'a*'b*'c*'d*'e*'f> with
                member __.Property ((a,b,c,d,e,f)) = a .& b .& c .& d .& e .& f}
        static member List() =
            { new Testable<list<'a>> with
                member __.Property l = List.fold (.&) (property <| List.head l) (List.tail l) }

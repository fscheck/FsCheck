namespace FsCheck

open System

open FsCheck.FSharp
open FsCheck.Internals

[<NoComparison; RequireQualifiedAccess>]
type Outcome = 
    | Failed of exn
    | Passed
    | Rejected 
    /// determines for which Outcome the result should be shrunk, or shrinking should continue.
    member internal x.Shrink = match x with Failed _ -> true | _ -> false 

///The result of one execution of a property.
[<NoComparison>]
type Result = 
    {   Outcome     : Outcome
        Stamp       : list<string>
        Labels      : Set<string>
        Arguments   : list<obj> } 
    ///Returns a new result that is Succeeded if and only if both this
    ///and the given Result are Succeeded.
    static member ResAnd l r = 
        //printfn "And of l %A and r %A" l.Outcome r.Outcome
        match (l.Outcome,r.Outcome) with
        | (Outcome.Failed _,_) -> l //here a potential Failed in r is thrown away...
        | (_,Outcome.Failed _) -> r
        | (_,Outcome.Passed) -> l
        | (Outcome.Passed,_) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever
    static member ResOr l r =
        match (l.Outcome, r.Outcome) with
        | (Outcome.Failed _,_) -> r
        | (_,Outcome.Failed _) -> l
        | (Outcome.Passed,_) -> l
        | (_,Outcome.Passed) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever

type ResultContainer = 
    | Value of Result
    | Future of Threading.Tasks.Task<Result>
    static member (&&&) (l,r) = 
        match (l,r) with
        | (Value vl,Value vr) -> Result.ResAnd vl vr |> Value
        | (Future tl,Value vr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.ResAnd x.Result vr) |> Future
        | (Value vl,Future tr) -> tr.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.ResAnd x.Result vl) |> Future
        | (Future tl,Future tr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> 
            tr.ContinueWith (fun (y :Threading.Tasks.Task<Result>) -> Result.ResAnd x.Result y.Result)) |> Threading.Tasks.TaskExtensions.Unwrap |> Future
    static member (|||) (l,r) =
        match (l,r) with
        | (Value vl,Value vr) -> Result.ResOr vl vr |> Value
        | (Future tl,Value vr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.ResOr x.Result vr) |> Future
        | (Value vl,Future tr) -> tr.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> Result.ResOr x.Result vl) |> Future
        | (Future tl,Future tr) -> tl.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> 
            tr.ContinueWith (fun (y :Threading.Tasks.Task<Result>) -> Result.ResOr x.Result y.Result)) |> Threading.Tasks.TaskExtensions.Unwrap |> Future

module internal Res =

    let private result =
      { Outcome     = Outcome.Rejected
        Stamp       = []
        Labels      = Set.empty
        Arguments   = []
      }

    let failed = { result with Outcome = Outcome.Failed (exn "Expected true, got false.") } |> Value

    let exc e = { result with Outcome = Outcome.Failed e } |> Value

    let succeeded = { result with Outcome = Outcome.Passed } |> Value

    let rejected = { result with Outcome = Outcome.Rejected } |> Value

    let rejectedV = { result with Outcome = Outcome.Rejected }

    let future (t :Threading.Tasks.Task<Outcome>) = t.ContinueWith (fun (x :Threading.Tasks.Task<Outcome>) -> { result with Outcome = x.Result }) |> Future

///A Property can be checked by FsCheck.
type Property = private Property of Gen<Shrink<ResultContainer>> with
    static member internal GetGen (Property g) = g

module private Testable =

    exception DiscardException

    open FsCheck.Internals.TypeClass
                   
    type ITestable<'a> =
        abstract Property : 'a -> Property
    
    type Testables = class end
     
    let internal testableTC = 
        (lazy
            let empty = TypeClass<ITestable<obj>>.New()
            empty.DiscoverAndMerge(onlyPublic=false,instancesType=typeof<Testables>)).Force()
        
    let property<'a> p = testableTC.InstanceFor<'a,ITestable<'a>>().Property p

    module internal Prop = 
    
        let ofRoseResult t : Property = Property <| gen { return t }

        let ofResult (r:ResultContainer) : Property = 
            ofRoseResult <| Shrink.ofValue r
         
        let ofBool b = ofResult <| if b then Res.succeeded else Res.failed
        
        let ofTaskBool (b:Threading.Tasks.Task<bool>) = 
            ofResult <| Res.future (b.ContinueWith (fun (x:Threading.Tasks.Task<bool>) -> 
                match (x.IsCanceled, x.IsFaulted) with
                    | (false,false) -> if x.Result then Outcome.Passed else Outcome.Failed <| exn "Expected true, got false."
                    | (_,true) -> Outcome.Failed x.Exception
                    | (true,_) -> Outcome.Failed <| exn "The Task was canceled."))
        
        let ofTask (b:Threading.Tasks.Task) = 
            ofResult <| Res.future (b.ContinueWith (fun (x:Threading.Tasks.Task) -> 
                match (x.IsCanceled, x.IsFaulted) with
                    | (false,false) -> Outcome.Passed
                    | (_,true) -> Outcome.Failed x.Exception
                    | (true,_) -> Outcome.Failed <| exn "The Task was canceled."))

        let mapRoseResult f a = property a |> Property.GetGen |> Gen.map f |> Property

        let mapResult f = mapRoseResult (Shrink.map f)

        let safeForce (body:Lazy<_>) =
            try
                property body.Value
            with
            | :? DiscardException -> ofResult Res.rejected
            | e -> ofResult (Res.exc e)

    let private shrinking shrink generatedValue propertyFun =
        let promoteRose (m:Shrink<Gen<'T>>) : Gen<Shrink<'T>> = 
            Gen.promote (fun runner -> Shrink.map runner m)

        propertyFun
        |> Shrink.ofShrinker shrink generatedValue
        |> promoteRose
        |> Gen.map Shrink.join
    
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
        |> Gen.map (Shrink.map (argument a))


    let forAll (arb:Arbitrary<_>) body : Property =
        let generator = arb.Generator
        let shrinker = arb.Shrinker
        gen { let! a = generator
              return! shrinking shrinker a (fun a' -> evaluate body a')}
        |> Property

    let private combine f a b:Property = 
        let pa = property a |> Property.GetGen
        let pb = property b |> Property.GetGen
        Gen.map2 (Shrink.map2 f) pa pb
        |> Property

    let (.&) l r = combine (&&&) l r

    let (.|) l r = combine (|||) l r

    type Testables with
        static member Unit() =
            { new ITestable<unit> with
                member __.Property _ = Prop.ofResult Res.succeeded }
        static member Bool() =
            { new ITestable<bool> with
                member __.Property b = Prop.ofBool b }
        static member TaskBool() =
            { new ITestable<Threading.Tasks.Task<bool>> with
                member __.Property b = Prop.ofTaskBool b }
        static member Task() =
            { new ITestable<Threading.Tasks.Task> with
                member __.Property b = Prop.ofTask b }
        static member AsyncBool() =
            { new ITestable<Async<bool>> with
                member __.Property b = Prop.ofTaskBool <| Async.StartAsTask b }
        static member Async() =
            { new ITestable<Async<unit>> with
                member __.Property b = Prop.ofTask <| Async.StartAsTask b }
        static member Lazy() =
            { new ITestable<Lazy<'a>> with
                member __.Property b =
                    let promoteLazy (m:Lazy<_>) = 
                        Gen.promote (fun runner -> lazy (runner m.Value) |> Shrink.ofLazy |> Shrink.join)
                    promoteLazy (lazy (Prop.safeForce b |> Property.GetGen)) |> Property } 
        static member Result() =
            { new ITestable<Result> with
                member __.Property res = Prop.ofResult <| Value res }
        static member ResultContainer() =
            { new ITestable<ResultContainer> with
                member __.Property resC = Prop.ofResult resC }
        static member Property() =
            { new ITestable<Property> with
                member __.Property prop = prop }
        static member Gen() =
            { new ITestable<Gen<'a>> with
                member __.Property gena = gen { let! a = gena in return! property a |> Property.GetGen } |> Property }
        static member RoseResult() =
            { new ITestable<Shrink<ResultContainer>> with
                member __.Property rosea = gen { return rosea } |> Property }
        static member Arrow() =
            { new ITestable<('a->'b)> with
                member __.Property f = forAll Arb.from f }
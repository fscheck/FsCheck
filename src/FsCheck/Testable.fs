namespace FsCheck

open System

open System.Threading.Tasks

open FsCheck.FSharp
open FsCheck.Internals

[<NoComparison; RequireQualifiedAccess>]
type Outcome = 
    | Failed of exn
    | Passed
    | Rejected 
    /// determines for which Outcome the result should be shrunk, or shrinking should continue.
    member internal outcome.Shrink = match outcome with Failed _ -> true | _ -> false 

///The result of one execution of a property.
[<NoComparison>]
type Result = 
    {   Outcome     : Outcome
        Stamp       : list<string>
        Labels      : Set<string>
        Arguments   : list<obj> } 
    ///Returns a new result that is Passed if and only if both this
    ///and the given Result are Passed.
    static member internal ResAnd(l, r) = 
        match (l.Outcome,r.Outcome) with
        | (Outcome.Failed _,_) -> l //here a potential Failed in r is thrown away...
        | (_,Outcome.Failed _) -> r
        | (_,Outcome.Passed) -> l
        | (Outcome.Passed,_) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever
    static member internal ResOr(l, r) =
        match (l.Outcome, r.Outcome) with
        | (Outcome.Failed _,_) -> r
        | (_,Outcome.Failed _) -> l
        | (Outcome.Passed,_) -> l
        | (_,Outcome.Passed) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever

    static member (&&&) (l,r) = Result.ResAnd(l, r)
    static member (|||) (l,r) = Result.ResOr(l, r)


module internal Res =

    let private result =
      { Outcome     = Outcome.Rejected
        Stamp       = []
        Labels      = Set.empty
        Arguments   = []
      }

    let failedException e = { result with Outcome = Outcome.Failed e }

    let failedFalse = failedException (exn "Expected true, got false.")

    let failedCancelled = failedException (exn "The Task was canceled.")

    let passed = { result with Outcome = Outcome.Passed }

    let rejected = { result with Outcome = Outcome.Rejected }

    let ofBool b = if b then passed else failedFalse


///A Property can be checked by FsCheck.
type Property = private Property of (IArbMap -> Gen<Shrink<Result>>) with
    static member internal GetGen arbMap (Property g) = g arbMap

module private Testable =

    exception DiscardException

    open FsCheck.Internals.TypeClass
                   
    type ITestable<'T> =
        abstract Property : 'T -> Property
    
    type Testables = class end
     
    let internal testableTC = 
        (lazy
            let empty = TypeClass<ITestable<obj>>.New()
            empty.DiscoverAndMerge(onlyPublic=false,instancesType=typeof<Testables>)).Force()
        
    let property<'a> p = testableTC.InstanceFor<'a,ITestable<'a>>().Property p

    module internal Prop = 
    
        let ofShrinkResult t : Property = 
            fun _  -> Gen.constant t
            |> Property

        let ofResult (r:Result) : Property = 
            r
            |> Shrink.ofValue
            |> ofShrinkResult
         
        let ofBool b =
            Res.ofBool b
            |> ofResult
        
        let ofTaskBool (b:Task<bool>) :Property = 
            try 
                b.Wait()
            with
                | :? AggregateException -> ()

            match (b.IsCanceled, b.IsFaulted) with
                | (false,false) -> Res.ofBool b.Result
                | (_,true) -> Res.failedException b.Exception
                | (true,_) -> Res.failedCancelled
            |> ofResult

        let ofTaskProperty (b:Task<Property>) :Property = 
            try 
                b.Wait()
            with
                | :? AggregateException -> ()

            match (b.IsCanceled, b.IsFaulted) with
                | (false,false) -> b.Result
                | (_,true) -> Res.failedException b.Exception |> ofResult
                | (true,_) -> Res.failedCancelled |> ofResult
        
        let ofTask (b:Task) :Property = 
            try 
                b.Wait()
            with
                | :? AggregateException -> ()

            match (b.IsCanceled, b.IsFaulted) with
                | (false,false) -> Res.passed
                | (_,true) -> Res.failedException b.Exception
                | (true,_) -> Res.failedCancelled
            |> ofResult

        let mapShrinkResult (f:Shrink<Result> -> _) a = 
            fun arbMap -> 
                property a 
                |> Property.GetGen arbMap
                |> Gen.map f
            |> Property

        let mapResult f = mapShrinkResult (Shrink.map f)

        let safeForce (body:Lazy<_>) =
            try
                property body.Value
            with
            | :? DiscardException -> Res.rejected |> ofResult
            | e -> Res.failedException e |> ofResult

    let private shrinking shrink generatedValue propertyFun =
        let promoteRose (m:Shrink<Gen<'T>>) : Gen<Shrink<'T>> = 
            Gen.promote (fun runner -> Shrink.map runner m)

        propertyFun
        |> Shrink.ofShrinker shrink generatedValue
        |> promoteRose
        |> Gen.map Shrink.join
    
    let private evaluate body a arbMap =
        let argument a res = 
            { res with Arguments = (box a) :: res.Arguments }
        Prop.safeForce (lazy ( body a ))
        |> Property.GetGen arbMap
        |> Gen.map (Shrink.map (argument a))


    let forAll (arb:Arbitrary<_>) body : Property =
        fun arbMap ->
            let generator = arb.Generator
            let shrinker = arb.Shrinker
            gen { let! a = generator
                  return! shrinking shrinker a (fun a' -> evaluate body a' arbMap)}
        |> Property

    let private combine f a b:Property = 
        fun arbMap ->
            let pa = property a |> Property.GetGen arbMap
            let pb = property b |> Property.GetGen arbMap
            Gen.map2 (Shrink.map2 f) pa pb
        |> Property

    let (.&) l r = combine (&&&) l r

    let (.|) l r = combine (|||) l r

    type Testables with
        static member Unit() =
            { new ITestable<unit> with
                member __.Property _ = Prop.ofResult Res.passed }
        static member Bool() =
            { new ITestable<bool> with
                member __.Property b = Prop.ofBool b }
        static member TaskBool() =
            { new ITestable<Task<bool>> with
                member __.Property b = Prop.ofTaskBool b }
        static member TaskProperty() =
            { new ITestable<Task<Property>> with
                member __.Property b = Prop.ofTaskProperty b }
        static member Task() =
            { new ITestable<Task> with
                member __.Property b = Prop.ofTask b }
        static member TaskGeneric() =
            { new ITestable<Task<'T>> with
                member __.Property b = Prop.ofTask (b :> Task) }
        static member AsyncBool() =
            { new ITestable<Async<bool>> with
                member __.Property b = Prop.ofTaskBool <| Async.StartAsTask b }
        static member AsyncProperty() =
            { new ITestable<Async<Property>> with
                member __.Property b = Prop.ofTaskProperty <| Async.StartAsTask b }
        static member Async() =
            { new ITestable<Async<unit>> with
                member __.Property b = Prop.ofTask <| Async.StartAsTask b }
        static member Lazy() =
            { new ITestable<Lazy<'T>> with
                member __.Property b =
                    let promoteLazy (m:Lazy<_>) = 
                        Gen.promote (fun runner -> lazy (runner m.Value) |> Shrink.ofLazy |> Shrink.join)
                    fun arbMap -> promoteLazy (lazy (Prop.safeForce b |> Property.GetGen arbMap))
                    |> Property } 
        static member Result() =
            { new ITestable<Result> with
                member __.Property res = Prop.ofResult res }
        static member Property() =
            { new ITestable<Property> with
                member __.Property prop = prop }
        static member Gen() =
            { new ITestable<Gen<'a>> with
                member __.Property gena = 
                    fun arbMap -> gen { let! a = gena in return! property a |> Property.GetGen arbMap }
                    |> Property }
        static member RoseResult() =
            { new ITestable<Shrink<Result>> with
                member __.Property rosea =
                    fun arbMap -> gen { return rosea }
                    |> Property }
        static member Arrow() =
            { new ITestable<('T->'U)> with
                member __.Property f =
                    fun (arbMap:IArbMap) ->
                        let arb = arbMap.ArbFor<'T>()
                        forAll arb f
                        |> Property.GetGen arbMap
                    |> Property }
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
    static member private MergeStampsAndLabels(l, r) =
        { l with
            Stamp = List.append l.Stamp r.Stamp
            Labels = Set.union l.Labels r.Labels }
    ///Returns a new result that is Passed if and only if both this
    ///and the given Result are Passed.
    static member internal ResAnd(l, r) = 
        match (l.Outcome,r.Outcome) with
        | (Outcome.Failed _,Outcome.Failed _) -> Result.MergeStampsAndLabels(l, r)
        | (Outcome.Failed _,_) -> l
        | (_,Outcome.Failed _) -> r
        | (_,Outcome.Rejected) -> r
        | (Outcome.Rejected,_) -> l
        | (Outcome.Passed,Outcome.Passed) -> Result.MergeStampsAndLabels(l, r)
    static member internal ResOr(l, r) =
        match (l.Outcome, r.Outcome) with
        | (Outcome.Failed _,Outcome.Failed _) -> Result.MergeStampsAndLabels(l, r)
        | (Outcome.Failed _,_) -> r
        | (_,Outcome.Failed _) -> l
        | (Outcome.Passed,Outcome.Passed) -> Result.MergeStampsAndLabels(l, r)
        | (Outcome.Passed,_) -> l
        | (_,Outcome.Passed) -> r
        | (Outcome.Rejected,Outcome.Rejected) -> l //or r, whatever

type ResultContainer = 
    | Value of Result
    | Future of Task<Result>
    static member private MapResult2(f, l, r) =
        match (l,r) with
        | (Value vl,Value vr) -> f (vl, vr) |> Value
        | (Future tl,Value vr) -> tl.ContinueWith (fun (x :Task<Result>) -> f (x.Result, vr)) |> Future
        | (Value vl,Future tr) -> tr.ContinueWith (fun (x :Task<Result>) -> f (vl, x.Result)) |> Future
        | (Future tl,Future tr) -> tl.ContinueWith (fun (x :Task<Result>) -> 
            tr.ContinueWith (fun (y :Task<Result>) -> f (x.Result, y.Result))) |> TaskExtensions.Unwrap |> Future
    static member (&&&) (l,r) = ResultContainer.MapResult2(Result.ResAnd, l, r)
    static member (|||) (l,r) = ResultContainer.MapResult2(Result.ResOr, l, r)


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
type Property = private Property of (IArbMap -> Gen<Shrink<ResultContainer>>) with
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

        let ofResult (r:ResultContainer) : Property = 
            r
            |> Shrink.ofValue
            |> ofShrinkResult
         
        let ofBool b =
            Res.ofBool b
            |> Value
            |> ofResult
        
        let ofTask (b:Task) :Property = 
            b.ContinueWith (fun (x:Task) -> 
                match (x.IsCanceled, x.IsFaulted) with
                    | (false,false) -> Res.passed
                    | (_,true) -> Res.failedException x.Exception
                    | (true,_) -> Res.failedCancelled)
            |> Future
            |> ofResult

        let ofTaskGeneric (t : Task<'T>) : Property =
            Property (fun arbMap ->
                Gen.promote (fun runner ->
                    Shrink.ofValue (Future (
                        t.ContinueWith (fun (t : Task<'T>) ->
                            match t.IsCanceled, t.IsFaulted with
                            | _, true -> Task.FromResult (Res.failedException t.Exception)
                            | true, _ -> Task.FromResult Res.failedCancelled
                            | false, false ->
                                let prop = property t.Result
                                let gen = Property.GetGen arbMap prop
                                let shrink = runner gen

                                let value, shrinks = Shrink.getValue shrink
                                assert Seq.isEmpty shrinks
                                match value with
                                | Value result -> Task.FromResult result
                                | Future resultTask -> resultTask)
                        |> _.Unwrap()))))

        let mapShrinkResult (f:Shrink<ResultContainer> -> _) a = 
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
            | :? DiscardException -> Res.rejected |> Value |> ofResult
            | e -> Res.failedException e |> Value |> ofResult

    let private shrinking shrink generatedValue propertyFun =
        let promoteRose (m:Shrink<Gen<'T>>) : Gen<Shrink<'T>> = 
            Gen.promote (fun runner -> Shrink.map runner m)

        propertyFun
        |> Shrink.ofShrinker shrink generatedValue
        |> promoteRose
        |> Gen.map Shrink.join
    
    let private evaluate body a arbMap =
        let argument a res = 
            match res with
            | ResultContainer.Value r -> { r with Arguments = (box a) :: r.Arguments } |> Value
            | ResultContainer.Future t -> t.ContinueWith (fun (rt :Task<Result>) -> 
                let r = rt.Result
                { r with Arguments = (box a) :: r.Arguments }) |> Future
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
                member __.Property _ = Prop.ofResult (Value Res.passed) }
        static member Bool() =
            { new ITestable<bool> with
                member __.Property b = Prop.ofBool b }
        static member Task() =
            { new ITestable<Task> with
                member __.Property b = Prop.ofTask b }
        static member TaskGeneric() =
            { new ITestable<Task<'T>> with
                member __.Property t = Prop.ofTaskGeneric t }
        static member AsyncGeneric() =
            { new ITestable<Async<'T>> with
                member __.Property a = Prop.ofTaskGeneric <| Async.StartAsTask a }
        static member Lazy() =
            { new ITestable<Lazy<'T>> with
                member __.Property b =
                    let promoteLazy (m:Lazy<_>) = 
                        Gen.promote (fun runner -> lazy (runner m.Value) |> Shrink.ofLazy |> Shrink.join)
                    fun arbMap -> promoteLazy (lazy (Prop.safeForce b |> Property.GetGen arbMap))
                    |> Property } 
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
                member __.Property gena = 
                    fun arbMap -> gen { let! a = gena in return! property a |> Property.GetGen arbMap }
                    |> Property }
        static member RoseResult() =
            { new ITestable<Shrink<ResultContainer>> with
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
namespace FsCheck

open System

[<NoComparison; RequireQualifiedAccess>]
type Outcome = 
    | Failed of exn
    | Passed
    /// determines for which Outcome the result should be shrunk, or shrinking should continue.
    member internal x.Shrink = match x with Failed _ -> true | _ -> false 

///The result of one execution of a property.
[<NoComparison>]
type Result = 
    {   Outcome     : Outcome
        Stamp       : list<string>
        Labels      : Set<string>
        Arguments   : list<obj> } 
    ///Returns a new result that is Passed if and only if both this
    ///and the given Result are Passed.
    static member resAnd l r = 
        //printfn "And of l %A and r %A" l.Outcome r.Outcome
        match (l.Outcome,r.Outcome) with
        | (Outcome.Failed _,_) -> l //here a potential Failed in r is thrown away...
        | (_,Outcome.Failed _) -> r
        | _                    -> l // must be both success...but shouldn't other fields be merged?

    static member resOr l r =
        match (l.Outcome, r.Outcome) with
        | (Outcome.Failed _,_) -> r
        | (_,Outcome.Failed _) -> l
        | _                    -> l // must be both success...but shouldn't other fields be merged?


type ResultContainer = 
    | Value of Result
    | Future of Threading.Tasks.Task<Result>
    static member (&&&) (l,r) = 
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

    let emptyPassed =
      { Outcome     = Outcome.Passed
        Stamp       = []
        Labels      = Set.empty
        Arguments   = []
      }

    let failed = { emptyPassed with Outcome = Outcome.Failed (exn "Expected true, got false.") } |> Value

    let exc e = { emptyPassed with Outcome = Outcome.Failed e } |> Value

    let succeeded = emptyPassed |> Value

    let future (t :Threading.Tasks.Task<Outcome>) = t.ContinueWith (fun (x :Threading.Tasks.Task<Outcome>) -> { emptyPassed with Outcome = x.Result }) |> Future

///A Property can be checked by FsCheck.
type Property = private Property of Gen<ResultContainer> with static member internal GetGen (Property g) = g

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
    
        let ofResult (r:ResultContainer) : Property = 
            Property <| gen { return r }
         
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

        let mapResult f a = property a |> Property.GetGen |> Gen.map f |> Property

        let safeForce (body:Lazy<_>) =
            try
                property body.Value
            with
//            | :? DiscardException -> ofResult Res.rejected
            | e -> ofResult (Res.exc e)
    
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
  //          | :? DiscardException -> Prop.ofResult Res.rejected
            | e -> Prop.ofResult (Res.exc e)
        |> Property.GetGen 
        |> Gen.map (argument a)


    let forAll (arb:Gen<_>) body : Property =
        arb >>= evaluate body
        |> Property

    let private combine f a b:Property = 
        let pa = property a |> Property.GetGen
        let pb = property b |> Property.GetGen
        Gen.map2 f pa pb
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
                    let promoteLazy (m:Lazy<Gen<_>>) = 
                        fun ctx ->
                            { Generate = fun () ->
                                let (Gen g) = m.Value
                                (g ctx).Generate()
                              GetShrinks = fun () ->
                                let (Gen g) = m.Value
                                (g ctx).GetShrinks()
                            }
                        |> Gen
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
        static member Arrow() =
            { new Testable<('a->'b)> with
                member __.Property f = forAll Arb.generate f }
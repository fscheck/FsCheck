
namespace FsCheck.Test

module Property =

    open Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open System
    open System.Threading
    open System.Threading.Tasks
    open Swensen.Unquote
    
    let internal curry f = fun a b -> f (a,b)

    let internal curry2 f = fun a b c -> f (a,b,c)

    let internal generate<'T> = ArbMap.defaults |> ArbMap.generate<'T>

    type SymProp =  | Unit | Bool of bool | Exception
                    | ForAll of int * SymProp
                    | Implies of bool * SymProp
                    | Classify of bool * string * SymProp
                    | Collect of int * SymProp
                    | Label of string * SymProp
                    | And of SymProp * SymProp
                    | Or of SymProp * SymProp
                    | LazyProp of SymProp
                    | Task
                    | FaultedTask
                    | CancelledTask
                    | TaskProp of SymProp
                    | FaultedTaskProp of SymProp
                    | CancelledTaskProp of SymProp
                    | AsyncProp of SymProp
     
    let rec private symPropGen =
        let rec recGen size =
            match size with
            | 0 -> Gen.oneof [Gen.constant Unit; Gen.map Bool generate; Gen.constant Exception]
            | n when n>0 ->
                let subProp = recGen (size/2)
                Gen.oneof
                        [ Gen.map2 (curry ForAll) generate (subProp)
                        ; Gen.map2 (curry Implies) generate (subProp)
                        ; Gen.map2 (curry Collect) generate (subProp)
                        ; Gen.map3 (curry2 Classify) generate generate (subProp)
                        ; Gen.map2 (curry Label) generate (subProp)
                        ; Gen.map2 (curry And) (subProp) (subProp)
                        ; Gen.map2 (curry Or) (subProp) (subProp)
                        ; Gen.map LazyProp subProp
                        ; Gen.constant Task
                        ; Gen.constant FaultedTask
                        ; Gen.constant CancelledTask
                        ; Gen.map TaskProp subProp
                        ; Gen.map FaultedTaskProp subProp
                        ; Gen.map CancelledTaskProp subProp
                        ; Gen.map AsyncProp subProp
                        ]
            | _ -> failwith "symPropGen: size must be positive"
        Gen.sized recGen
                  
    let rec private determineResult prop =
        let result =
          { Outcome     = Outcome.Rejected
            Stamp       = []
            Labels       = Set.empty
            Arguments   = []
          }
        let addStamp stamp res = { res with Stamp = stamp :: res.Stamp }
        let addArgument arg res = { res with Arguments = arg :: res.Arguments }
        let addLabel label (res:Result) = { res with Labels = Set.add label res.Labels }
        let andCombine prop1 prop2 :Result = let (r1:Result,r2) = determineResult prop1, determineResult prop2 in Result.ResAnd(r1, r2)
        match prop with
        | Unit ->   { result with Outcome= Outcome.Passed }
        | Bool true -> { result with Outcome= Outcome.Passed }
        | Bool false -> { result with Outcome= Outcome.Failed (exn "")}
        | Exception | FaultedTask | FaultedTaskProp _ -> { result with Outcome= Outcome.Failed (InvalidOperationException() :> exn)}
        | ForAll (i,prop) -> determineResult prop |> addArgument i
        | Implies (true,prop) -> determineResult prop
        | Implies (false,_) -> { result with Outcome= Outcome.Rejected }
        | Classify (true,stamp,prop) -> determineResult prop |> addStamp stamp
        | Classify (false,_,prop) -> determineResult prop
        | Collect (i,prop) -> determineResult prop |> addStamp (sprintf "%A" i)
        | Label (l,prop) -> determineResult prop |> addLabel l
        | And (prop1, prop2) -> andCombine prop1 prop2
        | Or (prop1, prop2) -> let r1,r2 = determineResult prop1, determineResult prop2 in Result.ResOr(r1, r2)
        | LazyProp prop -> determineResult prop
        | Task -> { result with Outcome = Outcome.Passed }
        | CancelledTask | CancelledTaskProp _ -> { result with Outcome = Outcome.Failed (exn "The Task was canceled.") }
        | TaskProp prop | AsyncProp prop -> determineResult prop
        
    let rec private toProperty prop =
        match prop with
        | Unit -> Prop.ofTestable ()
        | Bool b -> Prop.ofTestable b
        | Exception -> Prop.ofTestable (lazy (raise <| InvalidOperationException()))
        | ForAll (i,prop) -> Prop.forAll (Gen.constant i |> Arb.fromGen) (fun _ -> toProperty prop)
        | Implies (b,prop) -> b ==> (toProperty prop)
        | Classify (b,stamp,prop) -> Prop.classify b stamp (toProperty prop)
        | Collect (i,prop) -> Prop.collect i (toProperty prop)
        | Label (l,prop) -> Prop.label l (toProperty prop)
        | And (prop1,prop2) -> (toProperty prop1) .&. (toProperty prop2)
        | Or (prop1,prop2) -> (toProperty prop1) .|. (toProperty prop2)
        | LazyProp prop -> Prop.ofTestable (lazy toProperty prop)
        | Task -> Prop.ofTestable Task.CompletedTask
        | FaultedTask -> Prop.ofTestable (Task.FromException (InvalidOperationException ()))
        | CancelledTask -> Prop.ofTestable (Task.FromCanceled (CancellationToken (canceled=true)))
        | FaultedTaskProp Unit -> Prop.ofTestable (Task.FromException<unit> (InvalidOperationException ()))
        | FaultedTaskProp (Bool _) -> Prop.ofTestable (Task.FromException<bool> (InvalidOperationException ()))
        | FaultedTaskProp (LazyProp (Bool _)) -> Prop.ofTestable (Task.FromException<Lazy<bool>> (InvalidOperationException ()))
        | FaultedTaskProp _ -> Prop.ofTestable (Task.FromException<Property> (InvalidOperationException ()))
        | CancelledTaskProp Unit -> Prop.ofTestable (Task.FromCanceled<unit> (CancellationToken (canceled=true)))
        | CancelledTaskProp (Bool _) -> Prop.ofTestable (Task.FromCanceled<bool> (CancellationToken (canceled=true)))
        | CancelledTaskProp (LazyProp (Bool _)) -> Prop.ofTestable (Task.FromCanceled<Lazy<bool>> (CancellationToken (canceled=true)))
        | CancelledTaskProp _ -> Prop.ofTestable (Task.FromCanceled<Property> (CancellationToken (canceled=true)))
        | TaskProp Unit -> Prop.ofTestable (Task.FromResult ())
        | TaskProp (Bool b) -> Prop.ofTestable (Task.FromResult b)
        | TaskProp (LazyProp prop) -> Prop.ofTestable (Task.FromResult (lazy toProperty prop))
        | TaskProp prop -> Prop.ofTestable (Task.FromResult (toProperty prop))
        | AsyncProp Unit -> Prop.ofTestable (async { return () })
        | AsyncProp (Bool b) -> Prop.ofTestable (async { return b })
        | AsyncProp (LazyProp prop) -> Prop.ofTestable (async { return lazy toProperty prop })
        | AsyncProp prop -> Prop.ofTestable (async { return toProperty prop })
    
    let private areSame (r0:Result) (r1:TestResult) =
        let testData =
            match r1 with 
            | TestResult.Passed (td,_) -> td
            | TestResult.Failed (td,_,_,_,_,_,_) -> td
            | TestResult.Exhausted td -> td

        match r0.Outcome, r1 with
        | Outcome.Failed _, TestResult.Failed(_,_,_,Outcome.Failed _,_,_,_) -> r0.Labels = testData.Labels
        | Outcome.Passed, TestResult.Passed _ -> (r0.Stamp |> Set.ofSeq) = (testData.Stamps |> Seq.collect snd |> Set.ofSeq)
        | Outcome.Rejected,TestResult.Exhausted _ -> true
        | _ -> false
    
    let rec private depth (prop:SymProp) =
        match prop with
        | Unit -> 0
        | Bool _ -> 0
        | Exception -> 0
        | ForAll (_,prop) -> 1 + (depth prop)
        | Implies (_,prop) -> 1 + (depth prop)
        | Classify (_,_,prop) -> 1 + (depth prop)
        | Collect (_,prop) -> 1 + (depth prop)
        | Label (_,prop) -> 1 + (depth prop)
        | And (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | Or (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | LazyProp prop -> 1 + (depth prop)
        | Task | FaultedTask | CancelledTask -> 0
        | TaskProp prop | AsyncProp prop | FaultedTaskProp prop | CancelledTaskProp prop -> 1 + depth prop

    module private TestResult =
        let areSame result1 result2 =
            match result1, result2 with
            | TestResult.Failed ({ Labels = labels1 },_,_,Outcome.Failed _,_,_,_), TestResult.Failed ({ Labels = labels2 },_,_,Outcome.Failed _,_,_,_) -> labels1 = labels2
            | TestResult.Passed ({ Stamps = stamps1 },_), TestResult.Passed ({ Stamps = stamps2 },_) -> (stamps1 |> Seq.collect snd |> Set.ofSeq) = (stamps2 |> Seq.collect snd |> Set.ofSeq)
            | TestResult.Exhausted _, TestResult.Exhausted _ -> true
            | _ -> false
    
    //can not be an anonymous type because of let mutable.
    type private GetResultRunner() =
        let mutable result = None
        member __.Result = result.Value
        interface IRunner with
            override __.OnStartFixture _ = ()
            override __.OnArguments (ntest,args, every) = 
                printf "%s" (every ntest args)
            override __.OnShrink(args, everyShrink) = 
                printf "%s" (everyShrink args)
            override __.OnFinished(_,testResult) = 
                result <- Some testResult

    let private checkResult (prop:Property) =
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(2)
        Check.One(config, prop)
        resultRunner.Result

    [<Property>]
    let DSL() = 
        Prop.forAll (Arb.fromGen symPropGen) (fun symprop ->
            let expected = determineResult symprop
            let resultRunner = GetResultRunner()
            let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(2)
            Check.One(config,toProperty symprop)
            let actual = resultRunner.Result
            areSame expected actual
            |> Prop.label (sprintf "\nexpected =\n%A\nactual =\n%A" expected actual)
            |> Prop.collect (depth symprop)
        )

    [<Property>]
    let ``Synchronous unit properties behave the same as asynchronous ones`` () =
        (
            checkResult (Prop.ofTestable ()),
            checkResult (Prop.ofTestable (async { return () }))
        ) ||> TestResult.areSame

    [<Property>]
    let ``Synchronous unit properties behave the same as task-asynchronous ones`` () =
        (
            checkResult (Prop.ofTestable ()),
            checkResult (Prop.ofTestable (Task.FromResult ()))
        ) ||> TestResult.areSame

    [<Property>]
    let ``Synchronous Boolean properties behave the same as asynchronous ones`` (b : bool) =
        (
            checkResult (Prop.ofTestable b),
            checkResult (Prop.ofTestable (async { return b }))
        ) ||> TestResult.areSame

    [<Property>]
    let ``Synchronous Boolean properties behave the same as task-asynchronous ones`` (b : bool) =
        (
            checkResult (Prop.ofTestable b),
            checkResult (Prop.ofTestable (Task.FromResult b))
        ) ||> TestResult.areSame

    [<Property>]
    let ``Synchronous properties behave the same as asynchronous ones`` (b : bool) =
        (
            checkResult (b |> Prop.label $"{b}"),
            checkResult (Prop.ofTestable (async { return b |> Prop.label $"{b}" }))
        ) ||> TestResult.areSame

    [<Property>]
    let ``Synchronous properties behave the same as task-asynchronous ones`` (b : bool) =
        (
            checkResult (b |> Prop.label $"{b}"),
            checkResult (Prop.ofTestable (Task.FromResult (b |> Prop.label $"{b}")))
        ) ||> TestResult.areSame

    [<Property(MaxTest=1)>]
    let ``Or of exception and success should be success``() =
        let a = Prop.ofTestable <| lazy failwith "crash"
        let b =  Prop.ofTestable true
        a .|. b

    [<Fact>]
    let ``Single test with stamp should have the stamp in output``() =
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        Check.One (config, true |> Prop.trivial true)
        test <@ match resultRunner.Result with
                | TestResult.Passed (d, _) -> Seq.toList d.Stamps = [ 100, ["trivial"] ]
                | TestResult.Failed _ -> false
                | TestResult.Exhausted _ -> false @>

    [<Fact>]
    let ``Task-asynchronous tests should be passable``() =
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        let tcs = TaskCompletionSource<unit>()
        tcs.SetResult(())
        Check.One (config, Prop.ofTestable (tcs.Task :> Task))
        test <@ match resultRunner.Result with
                | TestResult.Passed _  -> true
                | TestResult.Failed _ -> false
                | TestResult.Exhausted _ -> false @>

    [<Fact>]
    let ``Generic task-asynchronous tests should be passable``() =
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        let tcs = TaskCompletionSource<unit>()
        tcs.SetResult(())
        Check.One (config, Prop.ofTestable tcs.Task) // No upcast
        test <@ match resultRunner.Result with
                | TestResult.Passed _  -> true
                | TestResult.Failed _ -> false
                | TestResult.Exhausted _ -> false @>

    [<Fact>]
    let ``Task-asynchronous tests should be failable by raising an exception``() =
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        let tcs = TaskCompletionSource()
        tcs.SetException(exn "fail")
        Check.One (config, Prop.ofTestable (tcs.Task))
        test <@ match resultRunner.Result with
                | TestResult.Passed _ -> false
                | TestResult.Failed _ -> true
                | TestResult.Exhausted _ -> false @>

    [<Fact>]
    let ``Task-asynchronous tests should be failable by cancellation``() =
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        let tcs = TaskCompletionSource()
        tcs.SetCanceled()
        Check.One (config, Prop.ofTestable (tcs.Task))
        test <@ match resultRunner.Result with
                | TestResult.Passed _ -> false
                | TestResult.Failed _ -> true
                | TestResult.Exhausted _ -> false @>

    [<Fact>]
    let ``throws should fail on unexpected exception``() =
        let test() =
            (lazy invalidOp "boom")
            |> Prop.throws<ArgumentException, _>
            |> Prop.label "Expected ArgumentException"
        let actual = checkResult (Prop.ofTestable test)
        match actual with
        | TestResult.Failed (td,_,_,Outcome.Failed e,_,_,_) when (e :? InvalidOperationException) -> 
            if not (td.Labels.Contains("Expected ArgumentException")) then
                failwith "Expected label to be applied"
        | t -> failwithf "Expected failing test with exception, got %A" t

    [<Fact>]
    let ``And should merge stamps from both properties``() =
        let prop1 = true |> Prop.collect "Property 1"
        let prop2 = true |> Prop.collect "Property 2"
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        Check.One(config, prop1 .&. prop2)
        test <@ match resultRunner.Result with
                | TestResult.Passed (d, _) -> 
                    let stamps = d.Stamps |> Seq.collect snd |> Set.ofSeq
                    stamps.Contains("\"Property 1\"") && stamps.Contains("\"Property 2\"")
                | _ -> false @>

    [<Fact>]
    let ``And should merge classifications from both properties``() =
        let prop1 = true |> Prop.classify true "P1"
        let prop2 = true |> Prop.classify true "P2"
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        Check.One(config, prop1 .&. prop2)
        test <@ match resultRunner.Result with
                | TestResult.Passed (d, _) -> 
                    let stamps = d.Stamps |> Seq.collect snd |> Set.ofSeq
                    stamps.Contains("P1") && stamps.Contains("P2")
                | _ -> false @>

    [<Fact>]
    let ``And should merge labels from both failing properties``() =
        let prop1 = false |> Prop.label "L1"
        let prop2 = false |> Prop.label "L2"
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        Check.One(config, prop1 .&. prop2)
        test <@ match resultRunner.Result with
                | TestResult.Failed (d, _, _, _, _, _, _) -> 
                    d.Labels.Contains("L1") && d.Labels.Contains("L2")
                | _ -> false @>

    [<Fact>]
    let ``Or should merge stamps from both properties when both pass``() =
        let prop1 = true |> Prop.collect "Property 1"
        let prop2 = true |> Prop.collect "Property 2"
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        Check.One(config, prop1 .|. prop2)
        test <@ match resultRunner.Result with
                | TestResult.Passed (d, _) -> 
                    let stamps = d.Stamps |> Seq.collect snd |> Set.ofSeq
                    stamps.Contains("\"Property 1\"") && stamps.Contains("\"Property 2\"")
                | _ -> false @>

    [<Fact>]
    let ``Or should merge classifications from both properties when both pass``() =
        let prop1 = true |> Prop.classify true "P1"
        let prop2 = true |> Prop.classify true "P2"
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        Check.One(config, prop1 .|. prop2)
        test <@ match resultRunner.Result with
                | TestResult.Passed (d, _) -> 
                    let stamps = d.Stamps |> Seq.collect snd |> Set.ofSeq
                    stamps.Contains("P1") && stamps.Contains("P2")
                | _ -> false @>

    [<Fact>]
    let ``Or should merge labels from both failing properties when both fail``() =
        let prop1 = false |> Prop.label "L1"
        let prop2 = false |> Prop.label "L2"
        let resultRunner = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner).WithMaxTest(1)
        Check.One(config, prop1 .|. prop2)
        test <@ match resultRunner.Result with
                | TestResult.Failed (d, _, _, _, _, _, _) -> 
                    d.Labels.Contains("L1") && d.Labels.Contains("L2")
                | _ -> false @>

    [<Fact>]
    let ``And should merge stamps regardless of order``() =
        let prop1 = true |> Prop.collect "Property 1" |> Prop.classify true "P1"
        let prop2 = true |> Prop.collect "Property 2" |> Prop.classify true "P2"
        
        let resultRunner1 = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner1).WithMaxTest(1)
        Check.One(config, prop1 .&. prop2)
        
        let resultRunner2 = GetResultRunner()
        let config2 = Config.Quick.WithRunner(resultRunner2).WithMaxTest(1)
        Check.One(config2, prop2 .&. prop1)
        
        test <@ match resultRunner1.Result, resultRunner2.Result with
                | TestResult.Passed (d1, _), TestResult.Passed (d2, _) -> 
                    let stamps1 = d1.Stamps |> Seq.collect snd |> Set.ofSeq
                    let stamps2 = d2.Stamps |> Seq.collect snd |> Set.ofSeq
                    stamps1.Contains("\"Property 1\"") && stamps1.Contains("\"Property 2\"") &&
                    stamps1.Contains("P1") && stamps1.Contains("P2") &&
                    stamps2.Contains("\"Property 1\"") && stamps2.Contains("\"Property 2\"") &&
                    stamps2.Contains("P1") && stamps2.Contains("P2")
                | _ -> false @>

    [<Fact>]
    let ``Or should merge stamps regardless of order when both pass``() =
        let prop1 = true |> Prop.collect "Property 1" |> Prop.classify true "P1"
        let prop2 = true |> Prop.collect "Property 2" |> Prop.classify true "P2"
        
        let resultRunner1 = GetResultRunner()
        let config = Config.Quick.WithRunner(resultRunner1).WithMaxTest(1)
        Check.One(config, prop1 .|. prop2)
        
        let resultRunner2 = GetResultRunner()
        let config2 = Config.Quick.WithRunner(resultRunner2).WithMaxTest(1)
        Check.One(config2, prop2 .|. prop1)
        
        test <@ match resultRunner1.Result, resultRunner2.Result with
                | TestResult.Passed (d1, _), TestResult.Passed (d2, _) -> 
                    let stamps1 = d1.Stamps |> Seq.collect snd |> Set.ofSeq
                    let stamps2 = d2.Stamps |> Seq.collect snd |> Set.ofSeq
                    stamps1.Contains("\"Property 1\"") && stamps1.Contains("\"Property 2\"") &&
                    stamps1.Contains("P1") && stamps1.Contains("P2") &&
                    stamps2.Contains("\"Property 1\"") && stamps2.Contains("\"Property 2\"") &&
                    stamps2.Contains("P1") && stamps2.Contains("P2")
                | _ -> false @>



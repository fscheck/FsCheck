
namespace FsCheck.Test

module Property =

    open FsCheck
    open FsCheck.Xunit
    open System
    open Arb
    
    let internal curry f = fun a b -> f (a,b)

    let internal curry2 f = fun a b c -> f (a,b,c)

    type SymProp =  | Unit | Bool of bool | Exception
                    | ForAll of int * SymProp
                    | Implies of bool * SymProp
                    | Classify of bool * string * SymProp
                    | Collect of int * SymProp
                    | Label of string * SymProp
                    | And of SymProp * SymProp
                    | Or of SymProp * SymProp
                    | LazyProp of SymProp
                    | Tuple2 of SymProp * SymProp
                    | Tuple3 of SymProp * SymProp * SymProp //and 4,5,6
                    | List of SymProp list
     
    let rec private symPropGen =
        let rec recGen size =
            match size with
            | 0 -> Gen.oneof [Gen.constant Unit; Gen.map (Bool) generate; Gen.constant Exception]
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
                        ; Gen.map2 (curry Tuple2) subProp subProp
                        ; Gen.map3 (curry2 Tuple3) subProp subProp subProp
                        ; Gen.map List (Gen.resize 3 <| Gen.nonEmptyListOf subProp)
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
        let andCombine prop1 prop2 :Result = let (r1:Result,r2) = determineResult prop1, determineResult prop2 in r1 &&& r2
        match prop with
        | Unit ->   { result with Outcome=Outcome.True }
        | Bool true -> { result with Outcome=Outcome.True }
        | Bool false -> { result with Outcome=Outcome.False }
        | Exception  -> { result with Outcome=Outcome.Exception (InvalidOperationException() :> exn)}
        | ForAll (i,prop) -> determineResult prop |> addArgument i
        | Implies (true,prop) -> determineResult prop
        | Implies (false,_) -> { result with Outcome=Outcome.Rejected }
        | Classify (true,stamp,prop) -> determineResult prop |> addStamp stamp
        | Classify (false,_,prop) -> determineResult prop
        | Collect (i,prop) -> determineResult prop |> addStamp (sprintf "%A" i)
        | Label (l,prop) -> determineResult prop |> addLabel l
        | And (prop1, prop2) -> andCombine prop1 prop2
        | Or (prop1, prop2) -> let r1,r2 = determineResult prop1, determineResult prop2 in r1 ||| r2
        | LazyProp prop -> determineResult prop
        | Tuple2 (prop1,prop2) -> andCombine prop1 prop2
        | Tuple3 (prop1,prop2,prop3) -> (andCombine prop1 prop2) &&& (determineResult prop3)
        | List props -> List.fold (fun st p -> st &&& determineResult p) (List.head props |> determineResult) (List.tail props)
        
    let rec private toProperty prop =
        match prop with
        | Unit -> Prop.ofTestable ()
        | Bool b -> Prop.ofTestable b
        | Exception -> Prop.ofTestable (lazy (raise <| InvalidOperationException()))
        | ForAll (i,prop) -> Prop.forAll (Gen.constant i |> Arb.fromGen) (fun i -> toProperty prop)
        | Implies (b,prop) -> b ==> (toProperty prop)
        | Classify (b,stamp,prop) -> Prop.classify b stamp (toProperty prop)
        | Collect (i,prop) -> Prop.collect i (toProperty prop)
        | Label (l,prop) -> Prop.label l (toProperty prop)
        | And (prop1,prop2) -> (toProperty prop1) .&. (toProperty prop2)
        | Or (prop1,prop2) -> (toProperty prop1) .|. (toProperty prop2)
        | LazyProp prop -> toProperty prop
        | Tuple2 (prop1,prop2) -> (toProperty prop1) .&. (toProperty prop2)
        | Tuple3 (prop1,prop2,prop3) -> (toProperty prop1) .&. (toProperty prop2) .&. (toProperty prop3)
        | List props -> List.fold (fun st p -> st .&. toProperty p) (List.head props |> toProperty) (List.tail props)
    
    let private areSame (r0:Result) (r1:TestResult) =
        let testData =
            match r1 with 
            | TestResult.True (td,_) -> td
            | TestResult.False (td,_,_,_,_) -> td
            | TestResult.Exhausted td -> td

        match r0.Outcome, r1 with
        | Outcome.Timeout i, TestResult.False(_,_,_,Outcome.Timeout j,_) when i = j -> true && r0.Labels = testData.Labels
        | Outcome.Exception _, TestResult.False(_,_,_,Outcome.Exception _,_) -> true && r0.Labels = testData.Labels
        | Outcome.False, TestResult.False(_,_,_,Outcome.False,_) -> true && r0.Labels = testData.Labels
        | Outcome.True, TestResult.True _ -> true && (r0.Stamp |> Set.ofSeq) = (testData.Stamps |> Seq.map snd |> Seq.concat |> Set.ofSeq)
        | Outcome.Rejected,TestResult.Exhausted _ -> true
        | _ -> false
    
    let rec private depth (prop:SymProp) =
        match prop with
        | Unit -> 0
        | Bool b -> 0
        | Exception -> 0
        | ForAll (_,prop) -> 1 + (depth prop)
        | Implies (_,prop) -> 1 + (depth prop)
        | Classify (_,_,prop) -> 1 + (depth prop)
        | Collect (_,prop) -> 1 + (depth prop)
        | Label (_,prop) -> 1 + (depth prop)
        | And (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | Or (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | LazyProp prop -> 1 + (depth prop)
        | Tuple2 (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | Tuple3 (prop1,prop2,prop3) -> 1 + Math.Max(Math.Max(depth prop1, depth prop2),depth prop3)
        | List props -> 1 + List.fold (fun a b -> Math.Max(a, depth b)) 0 props
    
    //can not be an anonymous type because of let mutable.
    type private GetResultRunner() =
        let mutable result = None
        member x.Result = result.Value
        interface IRunner with
            override x.OnStartFixture t = ()
            override x.OnArguments (ntest,args, every) = 
                printf "%s" (every ntest args)
            override x.OnShrink(args, everyShrink) = 
                printf "%s" (everyShrink args)
            override x.OnFinished(name,testResult) = 
                result <- Some testResult

    [<Property>]
    let DSL() = 
        Prop.forAll (Arb.fromGen symPropGen) (fun symprop ->
            let expected = determineResult symprop
            let resultRunner = GetResultRunner()
            let config = { Config.Quick with Runner = resultRunner; MaxTest = 2  }
            Check.One(config,toProperty symprop)
            let actual = resultRunner.Result
            areSame expected actual
            |> Prop.label (sprintf "expected = %A - actual = %A" expected actual)
            |> Prop.collect (depth symprop)
        )
            
        
        


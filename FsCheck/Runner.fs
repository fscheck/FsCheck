#light

namespace FsCheck

[<AutoOpen>]
module Runner

open System
open System.Collections.Generic
open System.Reflection
open Random
open Microsoft.FSharp.Reflection
open Generator
open Property
open TypeClass
open Common

type TestData = { NumberOfTests: int; Stamps: seq<int * list<string>>}
type TestResult = 
    | True of TestData
    | False of TestData * list<obj> * option<Exception> //the arguments that produced the failed test
    | Exhausted of TestData
    
type private TestStep = 
    | Generated of list<obj>    //generated arguments (test not yet executed)
    | Passed of Result //list<string>    //passed, test number and stamps for this test
    | Falsified of Result //list<obj> * option<Exception>   //falsified the property with given arguments, potentially exception was thrown
    | Failed of Result                   //generated arguments did not pass precondition
    | Shrunk of Result
    | EndShrink of Result

///For implementing your own test runner.
type IRunner =
    ///Called whenever arguments are generated and after the test is run.
    abstract member OnArguments: int * list<obj> * (int -> list<obj> -> string) -> unit
    ///Called whenever all tests are done, either True, False or Exhausted.
    abstract member OnFinished: string * TestResult -> unit

///For configuring a run.
type Config = 
    { maxTest : int
      maxFail : int
      name    : string
      size    : float -> float  //determines size passed to the generator as funtion of the previous size. Rounded up.
                            //float is used to allow for smaller increases than 1.
                            //note: in QuickCheck, this is a function of the test number!
      every   : int -> list<obj> -> string  //determines what to print if new arguments args are generated in test n
      runner  : IRunner } //the test runner    

//foundFailure :: State -> P.Result -> [Rose (IO P.Result)] -> IO ()
//foundFailure st res ts =
//  do localMin st{ numTryShrinks = 0, isShrinking = True } res ts
//
let rec private shrinkResult (result:Result) (shrinks:seq<Rose<Result>>) =
    seq { if not (Seq.is_empty shrinks) then
            let (MkRose ((Lazy result'),shrinks')) = Seq.hd shrinks
            match result'.ok with
            | Some (Lazy false) -> yield Shrunk result'; yield! shrinkResult result' shrinks'
            | _                 -> printfn "no shrinkk: %A" result'.arguments; yield! shrinkResult result <| Seq.skip 1 shrinks
          else
            yield EndShrink result
    }


//localMin :: State -> P.Result -> [Rose (IO P.Result)] -> IO ()
//localMin st res [] =
//  do putLine (terminal st)
//       ( P.reason res
//      ++ " (after " ++ number (numSuccessTests st+1) "test"
//      ++ concat [ " and " ++ number (numSuccessShrinks st) "shrink"
//                | numSuccessShrinks st > 0
//                ]
//      ++ "):  "
//       )
//     callbackPostFinalFailure st res
//
//localMin st res (t : ts) =
//  do -- CALLBACK before_test
//     (res',ts') <- run t
//     putTemp (terminal st)
//       ( short 35 (P.reason res)
//      ++ " (after " ++ number (numSuccessTests st+1) "test"
//      ++ concat [ " and "
//               ++ show (numSuccessShrinks st)
//               ++ concat [ "." ++ show (numTryShrinks st) | numTryShrinks st > 0 ]
//               ++ " shrink"
//               ++ (if numSuccessShrinks st == 1
//                   && numTryShrinks st == 0
//                   then "" else "s")
//                | numSuccessShrinks st > 0 || numTryShrinks st > 0
//                ]
//      ++ ")..."
//       )
//     callbackPostTest st res'
//     if ok res' == Just False
//       then foundFailure st{ numSuccessShrinks = numSuccessShrinks st + 1 } res' ts'
//       else localMin st{ numTryShrinks = numTryShrinks st + 1 } res ts



    
    
let rec private test initSize resize rnd0 gen =
    seq { let rnd1,rnd2 = split rnd0
          let newSize = resize initSize
          let (MkRose (Lazy result,shrinks)) = generate (newSize |> round |> int) rnd2 gen |> unProp
          yield Generated result.arguments
          match result.ok with
            | None -> 
                yield Failed result 
            | Some (Lazy true) -> 
                yield Passed result//.stamp
                //yield! test newSize resize rnd1 gen
            | Some (Lazy false) -> 
                yield Falsified result//.arguments,result.exc)
                yield! shrinkResult result shrinks
                //yield! test newSize resize rnd1 gen
          yield! test newSize resize rnd1 gen
    }

let private testsDone config outcome ntest stamps =    
    let entry (n,xs) = (100 * n / ntest),xs
    let table = stamps 
                |> Seq.filter (fun l -> l <> []) 
                |> Seq.sort_by (fun x -> x) 
                |> Seq.group_by (fun x -> x) 
                |> Seq.map (fun (l, ls) -> (Seq.length ls, l))
                |> Seq.sort_by (fun (l, ls) -> l)
                |> Seq.map entry
                //|> Seq.to_list
                //|> display
    let testResult =
        match outcome with
            | Passed _ -> True { NumberOfTests = ntest; Stamps = table }
            | Falsified result (*(args,exc)*) -> False ({ NumberOfTests = ntest; Stamps = table }, result.arguments, result.exc)
            | Failed _ -> Exhausted { NumberOfTests = ntest; Stamps = table }
            | EndShrink result -> False ({ NumberOfTests = ntest; Stamps = table }, result.arguments, result.exc)
            | _ -> failwith "Test ended prematurely"
    config.runner.OnFinished(config.name,testResult)
    //Console.Write(message outcome + " " + any_to_string ntest + " tests" + table:string)

let private runner config prop = 
    let testNb = ref 0
    let failedNb = ref 0
    let lastStep = ref (Failed rejected)
    test 0.0 (config.size) (newSeed()) (property prop) |>
    Seq.take_while (fun step ->
        lastStep := step
        match step with
            | Generated args -> config.runner.OnArguments(!testNb, args, config.every); true//Console.Write(config.every !testNb args); true
            | Passed _ -> testNb := !testNb + 1; !testNb <> config.maxTest //stop if we have enough tests
            | Falsified result -> printfn "Falsified: %A" result.arguments; testNb := !testNb + 1; true //falsified, true to continue with shrinking
            | Failed _ -> failedNb := !failedNb + 1; !failedNb <> config.maxFail //failed, stop if we have too much failed tests
            | Shrunk result -> printfn "Shrunk: %A" result.arguments; true
            | EndShrink result -> printfn "LastShrink: %A" result.arguments; false )
    |> Seq.fold (fun acc elem ->
        match elem with
            | Passed result -> (result.stamp :: acc)
            | _ -> acc
    ) [] 
    |> testsDone config !lastStep !testNb

///A function that returns the default string that is printed as a result of the test.
let testFinishedToString name testResult = 
    let display l = match l with
                    | []  -> ".\n"
                    | [x] -> " (" + x + ").\n"
                    | xs  -> ".\n" + List.fold_left (fun acc x -> x + ".\n"+ acc) "" xs
    let rec intersperse sep l = match l with
                                | [] -> []
                                | [x] -> [x]
                                | x::xs -> x :: sep :: intersperse sep xs
    let entry (p,xs) = any_to_string p + "% " + (intersperse ", " xs |> Seq.to_array |> String.Concat)
    let stamps_to_string s = s |> Seq.map entry |> Seq.to_list |> display
    let name = (name+"-")
    let printArgs = List.map any_to_string >> intersperse "\n" >> List.reduce_left (+)
    match testResult with
    | True data -> sprintf "%sOk, passed %i tests%s" name data.NumberOfTests (data.Stamps |> stamps_to_string )
    | False (data, args, None) -> sprintf "%sFalsifiable, after %i tests: \n%s\n" name data.NumberOfTests (args |> printArgs) 
    | False (data, args, Some exc) -> sprintf "%sFalsifiable, after %i tests: \n%s\n with exception:\n%O" name data.NumberOfTests (args |> printArgs) exc
    | Exhausted data -> sprintf "%sArguments exhausted after %i tests%s" name data.NumberOfTests (data.Stamps |> stamps_to_string )

///A runner that simply prints results to the console.
let consoleRunner =
    { new IRunner with
        member x.OnArguments (ntest,args, every) =
            printf "%s" (every ntest args)
        member x.OnFinished(name,testResult) = 
            printf "%s" (testFinishedToString name testResult)
    }
       
///The quick configuration only prints a summary result at the end of the test.
let quick = { maxTest = 100
              maxFail = 1000
              name    = ""
              size    = fun prevSize -> prevSize + 0.5
              every   = fun ntest args -> "" 
              runner  = consoleRunner } 

///The verbose configuration prints each generated argument.
let verbose = 
    { quick with every = fun n args -> any_to_string n + ":\n" + (args |> List.fold_left (fun b a -> any_to_string a + "\n" + b) "")  }

// resolve fails if the generic type is only determined by the return type 
//(e.g., Array.zero_create) but that is easily fixed by additionally passing in the return type...
let rec private resolve (acc:Dictionary<_,_>) (a:Type, f:Type) =
    if f.IsGenericParameter then
        if not (acc.ContainsKey(f)) then acc.Add(f,a)
    else 
        if a.HasElementType then resolve acc (a.GetElementType(), f.GetElementType())
        Array.zip (a.GetGenericArguments()) (f.GetGenericArguments()) |>
        Array.iter (resolve acc)

let private invokeMethod (m:MethodInfo) args =
    let m = if m.ContainsGenericParameters then
                let typeMap = new Dictionary<_,_>()
                Array.zip args (m.GetParameters()) |> 
                Array.iter (fun (a,f) -> resolve typeMap (a.GetType(),f.ParameterType))  
                let actuals = 
                    m.GetGenericArguments() |> 
                    Array.map (fun formal -> typeMap.[formal])
                m.MakeGenericMethod(actuals)
            else 
                m
    m.Invoke(null, args)

let private hasTestableReturnType (m:MethodInfo) =
    m.ReturnType = typeof<bool> 
    || m.ReturnType = typeof<Lazy<bool>> 
    || m.ReturnType = typeof<Property>
    //TODO: add FastFuncs that return any of the above

//let rec private findFunctionArgumentTypes fType = 
//    if not (FSharpType.IsFunction fType) then  
//            ([],fType)
//    else
//        let dom,range = FSharpType.GetFunctionElements fType
//        let args,ret = findFunctionArgumentTypes range
//        (dom :: args,ret)
//
//let private invokeFunction f args = 
//    f.GetType().InvokeMember("Invoke", System.Reflection.BindingFlags.InvokeMethod, null, f, args)

//let private checkFunction config f = 
//    let genericMap = new Dictionary<_,_>()  
//    let args,ret = findFunctionArgumentTypes (f.GetType())  
//    let gen = args    
//                |> List.map(fun p -> (getGenerator genericMap p  :?> IGen).AsGenObject )
//                |> sequence
//                |> (fun gen -> gen.Map List.to_array)
//    let property = makeProperty (invokeFunction f) ret
//    checkProperty config (forAll gen property) |> ignore

let check config p = runner config (forAllShrink arbitrary shrink p)

let checkName name config p = runner { config with name = name } (forAll arbitrary p)

let private checkMethodInfo = typeof<Config>.DeclaringType.GetMethod("check",BindingFlags.Static ||| BindingFlags.Public)

let private arrayToTupleType (arr:Type[]) =
    if arr.Length = 0 then
        typeof<unit>
    elif arr.Length = 1 then
        arr.[0]
    else
        FSharpType.MakeTupleType(arr)

let private tupleToArray t = 
    let ttype = t.GetType()
    if FSharpType.IsTuple ttype then
        FSharpValue.GetTupleFields(t)
    else
        [|t|]

let checkAll config (t:Type) = 
    t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) |>
    Array.filter hasTestableReturnType |>
    Array.iter(fun m -> 
        let fromP = m.GetParameters() |> Array.map (fun p -> p.ParameterType) |> arrayToTupleType
        let toP = m.ReturnType
        let funType = FSharpType.MakeFunctionType(fromP, toP) 
        let funValue = FSharpValue.MakeFunction(funType, tupleToArray >> invokeMethod m)
        let c = {config with name = t.Name+"."+m.Name}
        let genericM = checkMethodInfo.MakeGenericMethod([|fromP;toP|])
        genericM.Invoke(null, [|box c; funValue|]) |> ignore
        )


///Check with the configuration 'quick'.  
let quickCheck p = p |> check quick
///Check with the configuration 'verbose'.
let verboseCheck p = p |> check verbose 

///Check with the configuration 'quick', and using the given name.
let quickCheckN name p = p |> checkName name quick

///Check with the configuration 'verbose', and using the given name.
let verboseCheckN name p = p |> checkName name verbose

///Check all properties in given type with configuration 'quick'
let quickCheckAll t = t |> checkAll quick
/// Check all properties in given type with configuration 'verbose'
let verboseCheckAll t = t |> checkAll verbose 



//necessary initializations
do newTypeClass<Arbitrary<_>>
do registerGenerators<Arbitrary.Arbitrary>()
do newTypeClass<Testable<_>>
do registerInstances<Testable<_>,Testable>()

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

type TestData = { NumberOfTests: int; NumberOfShrinks: int; Stamps: seq<int * list<string>>}
type TestResult = 
    | True of TestData
    | False of TestData 
                * list<obj>(*the original arguments that produced the failed test*)
                * list<obj>(*the shrunk arguments that produce a failed test*)
                * option<Exception>(*possibly exception that falsified the property*) 
    | Exhausted of TestData
    
type private TestStep = 
    | Generated of list<obj>    //generated arguments (test not yet executed)
    | Passed of Result          //one test passed
    | Falsified of Result       //falsified the property
    | Failed of Result          //generated arguments did not pass precondition
    | Shrink of Result          //shrunk falsified result succesfully
    | NoShrink of Result        //could not falsify given result; so unsuccesful shrink.
    | EndShrink of Result       //gave up shrinking; (possibly) shrunk result is given

///For implementing your own test runner.
type IRunner =
    ///Called whenever arguments are generated and after the test is run.
    abstract member OnArguments: int * list<obj> * (int -> list<obj> -> string) -> unit
    ///Called on a succesful shrink.
    abstract member OnShrink: list<obj> * (list<obj> -> string) -> unit
    ///Called whenever all tests are done, either True, False or Exhausted.
    abstract member OnFinished: string * TestResult -> unit

///For configuring a run.
type Config = 
    { MaxTest       : int
      MaxFail       : int
      Name          : string
      Size          : float -> float  //determines size passed to the generator as funtion of the previous size. Rounded up.
                            //float is used to allow for smaller increases than 1.
                            //note: in QuickCheck, this is a function of the test number!
      Every         : int -> list<obj> -> string  //determines what to print if new arguments args are generated in test n
      EveryShrink   : list<obj> -> string  //determines what to print every time a counter-example is succesfully shrunk
      Runner        : IRunner } //the test runner    

let private tryForce (v : Lazy<bool>) : bool = try v.Value
                                               with _ -> false

let rec private shrinkResult (result:Result) (shrinks:seq<Rose<Result>>) =
    seq { if not (Seq.is_empty shrinks) then
            let (MkRose ((Lazy result'),shrinks')) = Seq.hd shrinks
            match result'.Ok with
            //| Some (Lazy false) -> yield Shrink result'; yield! shrinkResult result' shrinks'
            | Some x -> if not (tryForce x) then 
                            yield Shrink result'; yield! shrinkResult result' shrinks'
                        else
                            yield NoShrink result'; yield! shrinkResult result <| Seq.skip 1 shrinks  
            | _      -> yield NoShrink result'; yield! shrinkResult result <| Seq.skip 1 shrinks
          else
            yield EndShrink result
    }
    
let rec private test initSize resize rnd0 gen =
    seq { let rnd1,rnd2 = split rnd0
          let newSize = resize initSize
          let (MkRose (Lazy result,shrinks)) = generate (newSize |> round |> int) rnd2 gen |> unProp
          yield Generated result.Arguments
          match result.Ok with
            | None ->   yield Failed result 
            | Some v -> if tryForce v then
                           yield Passed result
                        else
                           yield Falsified result
                           yield! shrinkResult result shrinks
//            | Some (Lazy true) -> 
//                yield Passed result
//            | Some (Lazy false) -> 
//                yield Falsified result
//                yield! shrinkResult result shrinks
          yield! test newSize resize rnd1 gen
    }

let private testsDone config outcome origArgs ntest nshrinks stamps =    
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
        let testData = { NumberOfTests = ntest; NumberOfShrinks = nshrinks; Stamps = table }
        match outcome with
            | Passed _ -> True testData
            | Falsified result -> False (testData, origArgs, result.Arguments, result.Exc)
            | Failed _ -> Exhausted testData
            | EndShrink result -> False (testData, origArgs, result.Arguments, result.Exc)
            | _ -> failwith "Test ended prematurely"
    config.Runner.OnFinished(config.Name,testResult)
    //Console.Write(message outcome + " " + any_to_string ntest + " tests" + table:string)

let private runner config prop = 
    //let (++) orig = orig := !orig + 1
    let testNb = ref 0
    let failedNb = ref 0
    let shrinkNb = ref 0
    let tryShrinkNb = ref 0
    let origArgs = ref []
    let lastStep = ref (Failed rejected)
    test 0.0 (config.Size) (newSeed()) (property (lazy prop)) |>
    Seq.take_while (fun step ->
        lastStep := step
        match step with
            | Generated args -> config.Runner.OnArguments(!testNb, args, config.Every); true//Console.Write(config.every !testNb args); true
            | Passed _ -> testNb := !testNb + 1; !testNb <> config.MaxTest //stop if we have enough tests
            | Falsified result -> origArgs := result.Arguments; testNb := !testNb + 1; true //falsified, true to continue with shrinking
            | Failed _ -> failedNb := !failedNb + 1; !failedNb <> config.MaxFail //failed, stop if we have too much failed tests
            | Shrink result -> tryShrinkNb := 0; shrinkNb := !shrinkNb + 1; config.Runner.OnShrink(result.Arguments, config.EveryShrink); true
            | NoShrink _ -> tryShrinkNb := !tryShrinkNb + 1; true
            | EndShrink _ -> false )
    |> Seq.fold (fun acc elem ->
        match elem with
            | Passed result -> (result.Stamp :: acc)
            | _ -> acc
    ) [] 
    |> testsDone config !lastStep !origArgs !testNb !shrinkNb


let rec private intersperse sep l = 
    match l with
    | [] -> []
    | [x] -> [x]
    | x::xs -> x :: sep :: intersperse sep xs

let private printArgs = List.map any_to_string >> intersperse "\n" >> List.reduce_left (+)

///A function that returns the default string that is printed as a result of the test.
let testFinishedToString name testResult =
    let pluralize nb = if nb = 1 then String.Empty else "s"
    let display l = match l with
                    | []  -> ".\n"
                    | [x] -> " (" + x + ").\n"
                    | xs  -> ".\n" + List.fold_left (fun acc x -> x + ".\n"+ acc) "" xs    
    let entry (p,xs) = any_to_string p + "% " + (intersperse ", " xs |> Seq.to_array |> String.Concat)
    let stamps_to_string s = s |> Seq.map entry |> Seq.to_list |> display
    let name = (name+"-")  
    match testResult with
    | True data -> 
        sprintf "%sOk, passed %i test%s%s" 
            name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stamps_to_string )
    | False (data, origArgs, args, None) -> 
        sprintf "%sFalsifiable, after %i test%s (%i shrink%s): \n%s\n" 
            name data.NumberOfTests (pluralize data.NumberOfTests) 
            data.NumberOfShrinks (pluralize data.NumberOfShrinks) (args |> printArgs) 
    | False (data, origArgs, args, Some exc) -> 
        sprintf "%sFalsifiable, after %i test%s (%i shrink%s): \n%s\n with exception:\n%O" 
            name data.NumberOfTests (pluralize data.NumberOfTests) 
            data.NumberOfShrinks (pluralize data.NumberOfShrinks) (args |> printArgs) exc
    | Exhausted data -> 
        sprintf "%sArguments exhausted after %i test%s%s" 
            name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stamps_to_string )

///A runner that simply prints results to the console.
let consoleRunner =
    { new IRunner with
        member x.OnArguments (ntest,args, every) =
            printf "%s" (every ntest args)
        member x.OnShrink(args, everyShrink) =
            printf "%s" (everyShrink args)
        member x.OnFinished(name,testResult) = 
            printf "%s" (testFinishedToString name testResult)
    }
       
///The quick configuration only prints a summary result at the end of the test.
let quick = { MaxTest       = 100
              MaxFail       = 1000
              Name          = ""
              Size          = fun prevSize -> prevSize + 0.5
              Every         = fun ntest args -> String.Empty
              EveryShrink   = fun args -> String.Empty
              Runner        = consoleRunner } 

///The verbose configuration prints each generated argument.
let verbose = 
    { quick with 
        Every       = (fun n args -> sprintf "%i:\n%s\n" n (printArgs args));
        EveryShrink = fun args -> sprintf "shrink:\n%s\n" <| printArgs args
        //any_to_string n + ":\n" + (args |> List.fold_left (fun b a -> any_to_string a + "\n" + b) "")  } 
    }

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

let checkName name config p = runner { config with Name = name } (forAll arbitrary p)

let private checkMethodInfo = typeof<Config>.DeclaringType.GetMethod("check",BindingFlags.Static ||| BindingFlags.Public)

let private arrayToTupleType (arr:Type[]) =
    if arr.Length = 0 then
        typeof<unit>
    elif arr.Length = 1 then
        arr.[0]
    else
        FSharpType.MakeTupleType(arr)

let private tupleToArray t = 
    if t = null then
        Array.empty
    elif FSharpType.IsTuple (t.GetType()) then
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
        let c = {config with Name = t.Name+"."+m.Name}
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

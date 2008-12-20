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

type TestData = { NumberOfTests: int; Stamps: seq<int * list<string>>}
type TestResult = 
    | True of TestData
    | False of TestData * list<obj> * option<Exception> //the arguments that produced the failed test
    | Exhausted of TestData
    
type private TestStep = 
    | Generated of list<obj>    //test number and generated arguments (test not yet executed)
    | Passed of list<string>    //passed, test number and stamps for this test
    | Falsified of list<obj> * option<Exception>   //falsified the property with given arguments, potentially exception was thrown
    | Failed                    //generated arguments did not pass precondition

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


let private (|Lazy|) (inp:Lazy<'a>) = inp.Force()             

let rec private test initSize resize rnd0 gen =
    seq { let rnd1,rnd2 = split rnd0
          let newSize = resize initSize
          let result = generate (newSize |> round |> int) rnd2 gen
          yield Generated result.arguments
          match result.ok with
            | None -> 
                yield Failed  
                yield! test newSize resize rnd1 gen
            | Some (Lazy true) -> 
                yield Passed result.stamp
                yield! test newSize resize rnd1 gen
            | Some (Lazy false) -> 
                yield Falsified (result.arguments,result.exc)
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
            | Falsified (args,exc) -> False ({ NumberOfTests = ntest; Stamps = table }, args, exc)
            | Failed _ -> Exhausted { NumberOfTests = ntest; Stamps = table }
            | _ -> failwith "Test ended prematurely"
    config.runner.OnFinished(config.name,testResult)
    //Console.Write(message outcome + " " + any_to_string ntest + " tests" + table:string)

let private runner config property = 
    let testNb = ref 0
    let failedNb = ref 0
    let lastStep = ref Failed
    test 0.0 (config.size) (newSeed()) (evaluate property) |>
    Seq.take_while (fun step ->
        lastStep := step
        match step with
            | Generated args -> config.runner.OnArguments(!testNb, args, config.every); true//Console.Write(config.every !testNb args); true
            | Passed _ -> testNb := !testNb + 1; !testNb <> config.maxTest //stop if we have enough tests
            | Falsified _ -> testNb := !testNb + 1; false //falsified, always stop
            | Failed -> failedNb := !failedNb + 1; !failedNb <> config.maxFail) |> //failed, stop if we have too much failed tests
    Seq.fold (fun acc elem ->
        match elem with
            | Passed stamp -> (stamp :: acc)
            | _ -> acc
    ) [] |>   
    testsDone config !lastStep !testNb

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
    match testResult with
        | True data -> sprintf "%sOk, passed %i tests%s" name data.NumberOfTests (data.Stamps |> stamps_to_string )
        | False (data, args, None) -> sprintf "%sFalsifiable, after %i tests: %A\n" name data.NumberOfTests args 
        | False (data, args, Some exc) -> sprintf "%sFalsifiable, after %i tests: %A\n with exception:\n%O" name data.NumberOfTests args exc
        | Exhausted data -> sprintf "%sArguments exhausted after %i tests%s" name data.NumberOfTests (data.Stamps |> stamps_to_string )


let consoleRunner =
    { new IRunner with
        member x.OnArguments (ntest,args, every) =
            printf "%s" (every ntest args)
        member x.OnFinished(name,testResult) = 
            printf "%s" (testFinishedToString name testResult)
    }
       
let private checkProperty config property = runner config property

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

let private makeProperty invoker returnType args = 
    if returnType = typeof<bool> then
        invoker args |> unbox<bool> |> propl
    elif returnType = typeof<Lazy<bool>> then
        invoker args |> unbox<Lazy<bool>> |> prop
    elif returnType = typeof<Property> then
        invoker args |> unbox<Property>
    else
        failwith "Invalid return type: must be either bool, Lazy<bool> or Property"

let private checkType config (t:Type) = 
    t.GetMethods((BindingFlags.Static ||| BindingFlags.Public)) |>
    Array.filter hasTestableReturnType |>
    Array.map(fun m -> 
        let genericMap = new Dictionary<_,_>()
        //this needs IGen cause can't cast Gen<anything> to Gen<obj> directly (no variance!)
        let gen = m.GetParameters() 
                    |> Array.map(fun p -> (getGenerator genericMap p.ParameterType :?> IGen).AsGenObject)
                    |> Array.to_list
                    |> sequence
                    |> (fun gen -> gen.Map List.to_array)
        let property = makeProperty (invokeMethod m) (m.ReturnType)
        checkProperty {config with name = t.Name+"."+m.Name} (forAll gen property)) |> ignore

let rec private findFunctionArgumentTypes fType = 
    if not (FSharpType.IsFunction fType) then  
            ([],fType)
    else
        let dom,range = FSharpType.GetFunctionElements fType
        let args,ret = findFunctionArgumentTypes range
        (dom :: args,ret)

let private invokeFunction f args = 
    f.GetType().InvokeMember("Invoke", System.Reflection.BindingFlags.InvokeMethod, null, f, args)

let private checkFunction config f = 
    let genericMap = new Dictionary<_,_>()  
    let args,ret = findFunctionArgumentTypes (f.GetType())  
    let gen = args    
                |> List.map(fun p -> (getGenerator genericMap p  :?> IGen).AsGenObject )
                |> sequence
                |> (fun gen -> gen.Map List.to_array)
    let property = makeProperty (invokeFunction f) ret
    checkProperty config (forAll gen property) |> ignore

///Check the given Property or members of the given class Type or the given function, depending
///on the type of the argument.
let check config (whatever:obj) =
    match whatever with
        | :? Property as p -> checkProperty config p
        | :? Type as t ->  checkType config t
        | f -> checkFunction config f

///Check with the configuration 'quick'.  
let quickCheck p = p |> check quick
///Check with the configuration 'verbose'.
let verboseCheck p = p |> check verbose

[<ObsoleteAttribute("Use quickCheck instead.")>]
let qcheck gen p = forAll gen p |> quickCheck

[<ObsoleteAttribute("Use verboseCheck instead.")>]
let vcheck gen p = forAll gen p |> verboseCheck
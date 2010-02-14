(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2010 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

[<AutoOpen>]
module Runner =

    open System
    open System.Collections.Generic
    open System.Reflection
    open Random
    open Microsoft.FSharp.Reflection
    open Generator
    open Property
    open TypeClass
    open Common

    type TestData = 
        { NumberOfTests: int
        ; NumberOfShrinks: int
        ; Stamps: seq<int * list<string>>
        ; Labels: Set<string>
        }

    type TestResult = 
        | True of TestData
        | False of TestData 
                    * list<obj>(*the original arguments that produced the failed test*)
                    * list<obj>(*the shrunk arguments that produce a failed test*)
                    * Outcome(*possibly exception or timeout that falsified the property*) 
                    * StdGen (*the seed used*)
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
          Replay        : StdGen option
          Name          : string
          Size          : float -> float  //determines size passed to the generator as funtion of the previous size. Rounded up.
                                //float is used to allow for smaller increases than 1.
                                //note: in QuickCheck, this is a function of the test number!
          Every         : int -> list<obj> -> string  //determines what to print if new arguments args are generated in test n
          EveryShrink   : list<obj> -> string  //determines what to print every time a counter-example is succesfully shrunk
          Runner        : IRunner } //the test runner    

    let rec private shrinkResult (result:Result) (shrinks:seq<Rose<Result>>) =
        seq { if not (Seq.isEmpty shrinks) then
                //result forced here
                let (MkRose ((Lazy result'),shrinks')) = Seq.head shrinks 
                if result'.Outcome.Shrink then yield Shrink result'; yield! shrinkResult result' shrinks'
                else yield NoShrink result'; yield! shrinkResult result <| Seq.skip 1 shrinks
              else
                yield EndShrink result
        }
        
    let rec private test initSize resize rnd0 gen =
        seq { let rnd1,rnd2 = split rnd0
              let newSize = resize initSize
              //printfn "Before generate"
              //result forced here!
              let (MkRose (Lazy result,shrinks)) = generate (newSize |> round |> int) rnd2 gen
              //printfn "After generate"
              //problem: since result.Ok is no longer lazy, we only get the generated args _after_ the test is run
              yield Generated result.Arguments
              match result.Outcome with
                | Rejected -> 
                    yield Failed result 
                | Outcome.True -> 
                    yield Passed result
                | o when o.Shrink -> 
                    yield Falsified result
                    yield! shrinkResult result shrinks
                | _ ->
                    yield Falsified result
                    yield EndShrink result
              yield! test newSize resize rnd1 gen
        }

    let private testsDone config outcome origArgs ntest nshrinks usedSeed stamps  =    
        let entry (n,xs) = (100 * n / ntest),xs
        let table = stamps 
                    |> Seq.filter (not << List.isEmpty) //  (fun l -> l <> []) 
                    |> Seq.sort  //_by (fun x -> x) 
                    |> Seq.groupBy (fun x -> x) 
                    |> Seq.map (fun (l, ls) -> (Seq.length ls, l))
                    |> Seq.sortBy fst // (fun (l, ls) -> l)
                    |> Seq.map entry
                    //|> Seq.toList
                    //|> display
        let testResult =
            let testData = { NumberOfTests = ntest; NumberOfShrinks = nshrinks; Stamps = table; Labels = Set.empty }
            match outcome with
                | Passed _ -> True testData
                | Falsified result -> False ({ testData with Labels=result.Labels}, origArgs, result.Arguments, result.Outcome, usedSeed)
                | Failed _ -> Exhausted testData
                | EndShrink result -> False ({ testData with Labels=result.Labels}, origArgs, result.Arguments, result.Outcome, usedSeed)
                | _ -> failwith "Test ended prematurely"
        config.Runner.OnFinished(config.Name,testResult)
        //Console.Write(message outcome + " " + any_to_string ntest + " tests" + table:string)

    let private runner config prop = 
        let testNb = ref 0
        let failedNb = ref 0
        let shrinkNb = ref 0
        let tryShrinkNb = ref 0
        let origArgs = ref []
        let lastStep = ref (Failed rejected)
        let seed = match config.Replay with None -> newSeed() | Some s -> s
        test 0.0 (config.Size) seed (property prop) |>
        Seq.takeWhile (fun step ->
            lastStep := step
            //printfn "%A" step
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
        |> testsDone config !lastStep !origArgs !testNb !shrinkNb seed

    let private printArgs = List.map (sprintf "%A") >> String.concat "\n" (*>> List.reduce_left (+)*)

    ///A function that returns the default string that is printed as a result of the test.
    let testFinishedToString name testResult =
        let pluralize nb = if nb = 1 then String.Empty else "s"
        let display l = match l with
                        | []  -> ".\n"
                        | [x] -> " (" + x + ").\n"
                        | xs  -> ".\n" + List.fold (fun acc x -> x + ".\n"+ acc) "" xs    
        let entry (p,xs) = sprintf "%A%s %s" p "%" (String.concat ", " xs)
        let stamps_to_string s = s |> Seq.map entry |> Seq.toList |> display
        let labels_to_string l = String.concat ", " l
        let maybePrintLabels (l:Set<_>) = 
            if l.Count > 0 then
                sprintf "Label%s of failing property: %s\n" (pluralize l.Count) (labels_to_string l)
            else
                String.Empty
        let name = if String.IsNullOrEmpty(name) then String.Empty else (name+"-")  
        match testResult with
        | True data -> 
            sprintf "%sOk, passed %i test%s%s" 
                name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stamps_to_string )
        | False (data, origArgs, args, Exception exc, usedSeed) -> 
            sprintf "%sFalsifiable, after %i test%s (%i shrink%s) (%A):\n" 
                name data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed
            + maybePrintLabels data.Labels  
            + sprintf "%s\n" (args |> printArgs)
            + sprintf "with exception:\n%O\n" exc
        | False (data, origArgs, args, Timeout i, usedSeed) -> 
            sprintf "%sTimeout of %i milliseconds exceeded, after %i test%s (%i shrink%s) (%A):\n" 
                name i data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed 
            + maybePrintLabels data.Labels 
            + sprintf "%s\n" (args |> printArgs)
        | False (data, origArgs, args, _, usedSeed) -> 
            sprintf "%sFalsifiable, after %i test%s (%i shrink%s) (%A):\n" 
                name data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed
            + maybePrintLabels data.Labels  
            + sprintf "%s\n" (args |> printArgs) 
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
                  Replay        = None
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



    ///Force this value to do the necessary initializations of typeclasses. Normally this initialization happens automatically. 
    ///In any case, it can be forced any number of times without problem.
    let init = lazy (   initArbitraryTypeClass.Value
                        do registerGenerators<Arbitrary.Arbitrary>()
                        initTestableTypeClass.Value
                        do registerInstances<Testable<_>,Testable>())


    let private hasTestableReturnType (m:MethodInfo) =
        do init.Value
        try
            getInstance (typedefof<Testable<_>>,m.ReturnType) |> ignore
            true
        with
            e -> false

    ///Check the given property p using the given Config.
    let check config p = 
        //every check, even the reflective one, passes through here. So:
        init.Value // should always work
        runner config (property p)

    //Check the given property p using the given Config, and the given test name.
    let checkName name config p = check { config with Name = name } p

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

    ///Check all public static methods on the given type that have a Testable return type(this includes let-bound functions in a module)
    let checkAll config (t:Type) = 
        printfn "--- Checking %s ---" t.Name
        t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) |>
        Array.filter hasTestableReturnType |>
        Array.iter(fun m -> 
            let fromP = m.GetParameters() |> Array.map (fun p -> p.ParameterType) |> arrayToTupleType
            let toP = m.ReturnType
            let funType = FSharpType.MakeFunctionType(fromP, toP) 
            let funValue = FSharpValue.MakeFunction(funType, tupleToArray >> Reflect.invokeMethod m None)
            let c = {config with Name = t.Name+"."+m.Name}
            let genericM = checkMethodInfo.MakeGenericMethod([|funType(*fromP;toP*)|])
            genericM.Invoke(null, [|box c; funValue|]) |> ignore
            )
        printf "\n"


    ///Check with the configuration 'quick'.  
    let quickCheck p = p |> check quick
    ///Check with the configuration 'verbose'.
    let verboseCheck p = p |> check verbose 

    ///Check with the configuration 'quick', and using the given name.
    let quickCheckN name p = p |> checkName name quick

    ///Check with the configuration 'verbose', and using the given name.
    let verboseCheckN name p = p |> checkName name verbose

    ///Check all public static methods on the given type that have a Testable return type with configuration 'quick'
    let quickCheckAll t = t |> checkAll quick
    /// Check all public static methods on the given type that have a Testable return type with configuration 'verbose'
    let verboseCheckAll t = t |> checkAll verbose 



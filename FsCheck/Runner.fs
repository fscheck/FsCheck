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

open Random

type TestData = 
    { NumberOfTests: int
      NumberOfShrinks: int
      Stamps: seq<int * list<string>>
      Labels: Set<string>
    }

type TestResult = 
    | True of TestData
    | False of TestData 
                * list<obj>(*the original arguments that produced the failed test*)
                * list<obj>(*the shrunk arguments that produce a failed test*)
                * Outcome(*possibly exception or timeout that falsified the property*) 
                * StdGen (*the seed used*)
    | Exhausted of TestData


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
    { ///The maximum number of tests that are run.
      MaxTest       : int
      ///The maximum number of tests where values are rejected, e.g. as the result of ==>
      MaxFail       : int
      ///If set, the seed to use to start testing. Allows reproduction of previous runs.
      Replay        : StdGen option
      ///Name of the test.
      Name          : string
      ///The size to use for the first test.
      StartSize     : int
      ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
      EndSize       : int
      ///What to print when new arguments args are generated in test n
      Every         : int -> list<obj> -> string
      ///What to print every time a counter-example is succesfully shrunk
      EveryShrink   : list<obj> -> string 
      ///A custom test runner, e.g. to integrate with a test framework like xUnit or NUnit. 
      Runner        : IRunner }

module Runner =

    open System
    open System.Collections.Generic
    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Gen
    open Testable
    open TypeClass
    open Common
   
    type private TestStep = 
        | Generated of list<obj>    //generated arguments (test not yet executed)
        | Passed of Result          //one test passed
        | Falsified of Result       //falsified the property
        | Failed of Result          //generated arguments did not pass precondition
        | Shrink of Result          //shrunk falsified result succesfully
        | NoShrink of Result        //could not falsify given result; so unsuccesful shrink.
        | EndShrink of Result       //gave up shrinking; (possibly) shrunk result is given

 
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
              let (MkRose (Lazy result,shrinks)) = Gen.eval (newSize |> round |> int) rnd2 gen
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
                    |> Seq.filter (not << List.isEmpty)
                    |> Seq.sort
                    |> Seq.groupBy (fun x -> x) 
                    |> Seq.map (fun (l, ls) -> (Seq.length ls, l))
                    |> Seq.sortBy fst
                    |> Seq.map entry

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
        let lastStep = ref (Failed Res.rejected)
        let seed = match config.Replay with None -> newSeed() | Some s -> s
        let increaseSizeStep = float (config.EndSize - config.StartSize) / float config.MaxTest
        test (float config.StartSize) ((+) increaseSizeStep) seed (property prop) 
        |> Seq.takeWhile (fun step ->
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

    let printArgs = List.map (sprintf "%A") >> String.concat "\n"

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
           

    ///Force this value to do the necessary initializations of typeclasses. Normally this initialization happens automatically. 
    ///In any case, it can be forced any number of times without problem.
    let init = lazy Gen.register<Default>()

    let private hasTestableReturnType (m:MethodInfo) =
        ignore init.Value
        try
            TestableTC.GetInstance m.ReturnType |> ignore
            true
        with
            e -> false

    let internal check config p = 
        //every check, even the reflective one, passes through here. So:
        ignore init.Value // should always work
        runner config (property p)

    let internal checkName name config p = check { config with Name = name } p

    let private checkMethodInfo = typeof<TestStep>.DeclaringType.GetMethod("check",BindingFlags.Static ||| BindingFlags.NonPublic)

    let private arrayToTupleType (arr:Type[]) =
        if arr.Length = 0 then
            typeof<unit>
        elif arr.Length = 1 then
            arr.[0]
        else
            FSharpType.MakeTupleType(arr)

    let private tupleToArray types tvalue  =
        match types with
        | [||] -> Array.empty
        | [|t|] -> [|tvalue|]
        | _ -> FSharpValue.GetTupleFields(tvalue) 

    let checkMethod config (m:MethodInfo) =
        let fromTypes = m.GetParameters() |> Array.map (fun p -> p.ParameterType) 
        let fromP = fromTypes |> arrayToTupleType
        let toP = m.ReturnType
        let funType = FSharpType.MakeFunctionType(fromP, toP)
        let invokeAndThrowInner (m:MethodInfo) o = 
            try
                Reflect.invokeMethod m None o
             with :? TargetInvocationException as e -> //this is just to avoid huge non-interesting stacktraces in the output
                raise (Reflect.preserveStackTrace  e.InnerException)
        let funValue = FSharpValue.MakeFunction(funType, (tupleToArray fromTypes) >> invokeAndThrowInner m)
        let genericM = checkMethodInfo.MakeGenericMethod([|funType(*fromP;toP*)|])
        genericM.Invoke(null, [|box config; funValue|]) |> ignore
        

    let internal checkAll config (t:Type) = 
        printfn "--- Checking %s ---" t.Name
        t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) |>
        Array.filter hasTestableReturnType |>
        Array.iter (fun m -> checkMethod {config with Name = t.Name+"."+m.Name} m)
        printf "\n"

open Runner
open System

type Config with
    ///The quick configuration only prints a summary result at the end of the test.
    static member Quick =  
            { MaxTest       = 100
              MaxFail       = 1000
              Replay        = None
              Name          = ""
              StartSize     = 1
              EndSize       = 100
              Every         = fun ntest args -> String.Empty
              EveryShrink   = fun args -> String.Empty
              Runner        = consoleRunner } 
    ///The verbose configuration prints each generated argument.
    static member Verbose = 
        { Config.Quick with 
            Every       = fun n args -> sprintf "%i:\n%s\n" n (printArgs args)
            EveryShrink = fun args -> sprintf "shrink:\n%s\n" (printArgs args)
        }
    ///The default configuration is the quick configuration.
    static member Default = Config.Quick

type Check =

    //Check the given property using the given config.
    static member One (config,property:'Testable) = check config property

    //Check the given property using the given config, and the given test name.
    static member One (name,config,property:'Testable) = check { config with Name = name } property

    ///Check one property with the quick configuration.  
    static member Quick (property:'Testable) = Check.One(Config.Quick,property)

    ///Check one property with the quick configuration, and using the given name.
    static member Quick(name,property:'Testable) = Check.One(name,Config.Quick,property)

    ///Check one property with the verbose configuration.
    static member Verbose (property:'Testable) = Check.One(Config.Verbose,property)

    ///Check one property with the verbose configuration, and using the given name.
    static member Verbose(name,property:'Testable) = Check.One(name,Config.Verbose,property)

    ///Check all public static methods on the given type that have a testable return type with the given configuration.
    ///This includes let-bound functions in a module.
    static member All(config,test) = checkAll config test

    ///Check all public static methods on the given type that have a testable return type with the given configuration.
    ///This includes let-bound functions in a module.
    static member All<'Test> config = Check.All(config,typeof<'Test>)

    ///Check all public static methods on the given type that have a testable return type with quick configuration
    static member QuickAll test = Check.All(Config.Quick,test)

    ///Check all public static methods on the given type that have a testable return type with quick configuration
    static member QuickAll<'Test>() = Check.All(Config.Quick,typeof<'Test>)

    /// Check all public static methods on the given type that have a testable return type with vthe erbose configuration
    static member VerboseAll<'Test>() = Check.All(Config.Verbose,typeof<'Test>)

    /// Check all public static methods on the given type that have a testable return type with vthe erbose configuration
    static member VerboseAll test = Check.All(Config.Verbose,test)


// -- removed stuff
//    [<Obsolete("Use Config.Quick instead.")>]
//    let quick = Config.Quick
//
//    [<Obsolete("Use Config.Verbose instead.")>]
//    let verbose = Config.Verbose

//    [<Obsolete("This function will be removed in the following version of FsCheck. Use Check.Quick instead.")>]
//    let quickCheck p = p |> check quick
//
//    [<Obsolete("This function will be removed in the following version of FsCheck. Use Check.Verbose instead.")>]
//    let verboseCheck p = p |> check verbose 
//
//    [<Obsolete("This function will be removed in the following version of FsCheck. Use Check.Quick instead.")>]
//    let quickCheckN name p = p |> checkName name quick
//
//    [<Obsolete("This function will be removed in the following version of FsCheck. Use Check.Verbose instead.")>]
//    let verboseCheckN name p = p |> checkName name verbose
//
//    [<Obsolete("This function will be removed in the following version of FsCheck. Use Check.AllQuick instead.")>]
//    let quickCheckAll t = t |> checkAll quick
//
//    [<Obsolete("This function will be removed in the following version of FsCheck. Use Check.AllVerbose instead.")>]
//    let verboseCheckAll t = t |> checkAll verbose    
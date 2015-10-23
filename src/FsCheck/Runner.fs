(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **  
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

open Random
open System

[<NoEquality;NoComparison>]
type TestData = 
    { NumberOfTests: int
      NumberOfShrinks: int
      Stamps: seq<int * list<string>>
      Labels: Set<string>
    }

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type TestResult = 
    | True of TestData
                * bool(*suppress output*)
    | False of TestData 
                * list<obj>(*the original arguments that produced the failed test*)
                * list<obj>(*the shrunk arguments that produce a failed test*)
                * Outcome(*possibly exception or timeout that falsified the property*) 
                * StdGen (*the seed used*)
    | Exhausted of TestData


///For implementing your own test runner.
type IRunner =
    ///Called before a group of properties on a type are checked.
    abstract member OnStartFixture: System.Type -> unit
    ///Called whenever arguments are generated and after the test is run.
    abstract member OnArguments: int * list<obj> * (int -> list<obj> -> string) -> unit
    ///Called on a succesful shrink.
    abstract member OnShrink: list<obj> * (list<obj> -> string) -> unit
    ///Called whenever all tests are done, either True, False or Exhausted.
    abstract member OnFinished: string * TestResult -> unit

///For configuring a run.
[<NoEquality;NoComparison>]
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
      ///If set, suppresses the output from the test if the test is successful.
      QuietOnSuccess: bool
      ///What to print when new arguments args are generated in test n
      Every         : int -> list<obj> -> string
      ///What to print every time a counter-example is succesfully shrunk
      EveryShrink   : list<obj> -> string 
      ///The Arbitrary instances on this class will be merged in back to front order, i.e. instances for the same generated type at the front
      ///of the list will override those at the back. The instances on Arb.Default are always known, and are at the back (so they can always be
      ///overridden)
      Arbitrary     : list<Type>
      ///A custom test runner, e.g. to integrate with a test framework like xUnit or NUnit. 
      Runner        : IRunner }

module Runner =

    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Testable
    open TypeClass
   
    [<NoEquality;NoComparison>]
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
              let result, shrinks =
                  try
                    let (MkRose (Lazy result,shrinks)) = Gen.eval (newSize |> round |> int) rnd2 gen
                    result, shrinks
                    //printfn "After generate"
                    //problem: since result.Ok is no longer lazy, we only get the generated args _after_ the test is run
                  with :? DiscardException -> 
                    Res.rejected, Seq.empty

              yield Generated result.Arguments

              match result.Outcome with
                | Outcome.Rejected -> 
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

    let private testsDone config testStep origArgs ntest nshrinks usedSeed stamps  =    
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
            match testStep with
                | Passed _ -> TestResult.True (testData, config.QuietOnSuccess)
                | Falsified result -> TestResult.False ({ testData with Labels=result.Labels}, origArgs, result.Arguments, result.Outcome, usedSeed)
                | Failed _ -> TestResult.Exhausted testData
                | EndShrink result -> TestResult.False ({ testData with Labels=result.Labels}, origArgs, result.Arguments, result.Outcome, usedSeed)
                | _ -> failwith "Test ended prematurely"
        config.Runner.OnFinished(config.Name,testResult)

    let private runner config prop = 
        let testNb = ref 0
        let failedNb = ref 0
        let shrinkNb = ref 0
        let tryShrinkNb = ref 0
        let origArgs = ref []
        let lastStep = ref (Failed Res.rejected)
        let seed = match config.Replay with None -> newSeed() | Some s -> s
        let increaseSizeStep = float (config.EndSize - config.StartSize) / float config.MaxTest
        test (float config.StartSize) ((+) increaseSizeStep) seed (property prop |> Property.GetGen) 
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

    let private newline = Environment.NewLine

    let argumentsToString = List.map (sprintf "%A") >> String.concat newline

    let onStartFixtureToString (t:Type) =
        sprintf "--- Checking %s ---%s" t.Name newline

    let private pluralize nb = if nb = 1 then String.Empty else "s"

    let private labels_to_string l = String.concat newline l

    let private maybePrintLabels (l:Set<_>) = 
        match l.Count with
        | 0 -> String.Empty
        | 1 -> sprintf "Label of failing property: %s%s" (labels_to_string l) newline
        | _ -> sprintf "Labels of failing property (one or more is failing):%s%s%s" newline (labels_to_string l) newline

    let onFailureToString name data originalArgs args usedSeed =
        sprintf "%sFalsifiable, after %i test%s (%i shrink%s) (%A):%s" 
                name data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed newline
            + maybePrintLabels data.Labels
            + sprintf "Original:%s%s%s" newline (argumentsToString originalArgs) newline
            + if (data.NumberOfShrinks > 0 ) then sprintf "Shrunk:%s%s%s" newline (argumentsToString args) newline else ""

    ///A function that returns the default string that is printed as a result of the test.
    let onFinishedToString name testResult =
        let display l = match l with
                        | []  -> sprintf ".%s" newline
                        | [x] -> sprintf " (%s).%s" x newline
                        | xs  -> sprintf ".%s%s" newline (List.fold (fun acc x -> x + "." + newline + acc) "" xs)
        let entry (p,xs) = sprintf "%A%s %s" p "%" (String.concat ", " xs)
        let stamps_to_string s = s |> Seq.map entry |> Seq.toList |> display
        let name = if String.IsNullOrEmpty(name) then String.Empty else (name+"-")  
        match testResult with
        | TestResult.True (data, suppressOutput) ->
            if suppressOutput then ""
            else sprintf "%sOk, passed %i test%s%s"
                    name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stamps_to_string)
        | TestResult.False (data, originalArgs, args, Outcome.Exception exc, usedSeed) -> 
            onFailureToString name data originalArgs args usedSeed
            + sprintf "with exception:%s%O%s" newline exc newline
        | TestResult.False (data, _, args, Outcome.Timeout i, usedSeed) -> 
            sprintf "%sTimeout of %i milliseconds exceeded, after %i test%s (%i shrink%s) (%A):%s" 
                name i data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed newline
            + maybePrintLabels data.Labels 
            + sprintf "%s%s" (args |> argumentsToString) newline
        | TestResult.False (data, originalArgs, args, _, usedSeed) -> 
            onFailureToString name data originalArgs args usedSeed
        | TestResult.Exhausted data -> 
            sprintf "%sArguments exhausted after %i test%s%s" 
                name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stamps_to_string )

    let onArgumentsToString n args = 
        sprintf "%i:%s%s%s" n newline (argumentsToString args) newline
    
    let onShrinkToString args =
        sprintf "shrink:%s%s%s" newline (argumentsToString args) newline

#if PCL
    let internal printf fmt = 
        Printf.kprintf Diagnostics.Debug.WriteLine fmt
#endif
    
    ///A runner that prints results to the standard output.
    let consoleRunner =
        { new IRunner with
            member __.OnStartFixture t =
                printf "%s" (onStartFixtureToString t)
            member __.OnArguments (ntest,args, every) =
                printf "%s" (every ntest args)
            member __.OnShrink(args, everyShrink) =
                printf "%s" (everyShrink args)
            member __.OnFinished(name,testResult) = 
                printf "%s" (onFinishedToString name testResult)
        }
           

    ///Force this value to do the necessary initializations of typeclasses. Normally this initialization happens automatically. 
    ///In any case, it can be forced any number of times without problem.
    [<Obsolete("Calling this should no longer be necessary - though please file an issue if you find a case where it is :)")>]
    let init = 
        Arb.defaultArbitrary |> ignore
        lazy ()

    let private hasTestableReturnType (m:MethodInfo) =
        try
            TestableTC.GetInstance m.ReturnType |> ignore
            true
        with
            _ -> false

    let internal check config p = 
        //save so we can restore after the run
        let defaultArbitrary = Arb.Arbitrary.Value
        let merge newT (existingTC:TypeClass<_>) = existingTC.DiscoverAndMerge(onlyPublic=true,instancesType=newT)
        Arb.Arbitrary.Value <- List.foldBack merge config.Arbitrary defaultArbitrary
        try
            runner config (property p)
        finally
            Arb.Arbitrary.Value <- defaultArbitrary

    let private checkMethodInfo = 
        typeof<TestStep>.DeclaringType.GetTypeInfo().DeclaredMethods 
        |> Seq.find (fun meth -> meth.IsStatic && meth.Name = "check")

    let private arrayToTupleType (arr:Type[]) =
        if arr.Length = 0 then
            typeof<unit>
        elif arr.Length = 1 then
            arr.[0]
        else
            FSharpType.MakeTupleType(arr)

    let private tupleToArray types tvalue =
        match types with
        | [||] -> Array.empty
        | [|_|] -> [|tvalue|]
        | _ -> FSharpValue.GetTupleFields(tvalue) 

    let internal checkMethod (config:Config) (m:MethodInfo) (target:obj option) =
        let fromTypes = m.GetParameters() |> Array.map (fun p -> p.ParameterType) 
        let fromP = fromTypes |> arrayToTupleType
        //we can check methods that return void or unit. We cannot add it to  to the Testable typeclass
        //since F# won't let us define that - System.Void can only be used in typeof<> expressions.
        //hence the translation here.
        let toP = if m.ReturnType = typeof<System.Void> then typeof<unit> else m.ReturnType
        let funType = FSharpType.MakeFunctionType(fromP, toP)
        let invokeAndThrowInner (m:MethodInfo) o = 
            try
                Reflect.invokeMethod m target o
             with :? TargetInvocationException as e -> //this is just to avoid huge non-interesting stacktraces in the output
                System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e.InnerException).Throw()
                failwithf "Should not get here - please report a bug"
                //raise (Reflect.preserveStackTrace  e.InnerException)
        let funValue = FSharpValue.MakeFunction(funType, (tupleToArray fromTypes) >> invokeAndThrowInner m)
        let genericM = checkMethodInfo.MakeGenericMethod([|funType|])
        genericM.Invoke(null, [|box config; funValue|]) |> ignore
        

    let internal checkAll config (t:Type) = 
        config.Runner.OnStartFixture t  
        t.GetRuntimeMethods() 
        |> Seq.filter (fun meth -> meth.IsStatic && meth.IsPublic)
        |> Seq.filter hasTestableReturnType 
        |> Seq.iter (fun m -> checkMethod {config with Name = t.Name+"."+m.Name} m None)
        printf "%s" newline

open Runner
open System
open System.Runtime.CompilerServices

type Config with
    ///The quick configuration only prints a summary result at the end of the test.
    static member Quick =  
            { MaxTest       = 100
              MaxFail       = 1000
              Replay        = None
              Name          = ""
              StartSize     = 1
              EndSize       = 100
              QuietOnSuccess = false
              Every         = fun _ _ -> String.Empty
              EveryShrink   = fun _ -> String.Empty
              Arbitrary     = []
              Runner        = consoleRunner } 

    ///The verbose configuration prints each generated argument.
    static member Verbose = 
        { Config.Quick with 
            Every       = fun n args -> sprintf "%i:%s%s%s" n Environment.NewLine (argumentsToString args) Environment.NewLine
            EveryShrink = fun args -> sprintf "shrink:%s%s%s" Environment.NewLine (argumentsToString args) Environment.NewLine
        }

    static member private throwingRunner =
        { new IRunner with
            member __.OnStartFixture t =
                printf "%s" (onStartFixtureToString t)
            member __.OnArguments (ntest,args, every) =
                printf "%s" (every ntest args)
            member __.OnShrink(args, everyShrink) =
                printf "%s" (everyShrink args)
            member __.OnFinished(name,testResult) = 
                match testResult with
                | TestResult.True _ -> printf "%s" (onFinishedToString name testResult)
                | _ -> failwithf "%s" (onFinishedToString name testResult)

        }

    ///Like the Quick configuration, only throws an exception with the error message if the test fails or is exhausted.
    ///Useful for use within other unit testing frameworks that usually adopt this methodolgy to signal failure.
    static member QuickThrowOnFailure =
        { Config.Quick with Runner = Config.throwingRunner }

    ///Like the Verbose configuration, only throws an exception with the error message if the test fails or is exhausted.
    ///Useful for use within other unit testing frameworks that usually adopt this methodolgy to signal failure.
    static member VerboseThrowOnFailure =
        { Config.Verbose with Runner = Config.throwingRunner }

    ///The default configuration is the quick configuration.
    static member Default = Config.Quick

[<AbstractClass;Sealed>]
type Check =

    ///Check the given property using the given config.
    static member One (config,property:'Testable) = check config property

    ///Check the given property using the given config, and the given test name.
    static member One (name,config,property:'Testable) = check { config with Name = name } property
    
    ///Check the given property identified by the given MethodInfo.
    static member Method (config,methodInfo:Reflection.MethodInfo, ?target:obj) = checkMethod config methodInfo target

    ///Check one property with the quick configuration.  
    static member Quick (property:'Testable) = Check.One(Config.Quick,property)

    ///Check one property with the quick configuration, and using the given name.
    static member Quick(name,property:'Testable) = Check.One(name,Config.Quick,property)

    ///Check one property with the quick configuration, and throw an exception if it fails or is exhausted.
    static member QuickThrowOnFailure(property:'Testable) = Check.One(Config.QuickThrowOnFailure,property)

    ///Check one property with the verbose configuration.
    static member Verbose (property:'Testable) = Check.One(Config.Verbose,property)

    ///Check one property with the verbose configuration, and using the given name.
    static member Verbose(name,property:'Testable) = Check.One(name,Config.Verbose,property)

    ///Check one property with the verbose configuration, and throw an exception if it fails or is exhausted.
    static member VerboseThrowOnFailure(property:'Testable) = Check.One(Config.VerboseThrowOnFailure,property)

    ///Check all public static methods on the given type that have a testable return type with the given configuration.
    ///This includes let-bound functions in a module.
    static member All(config,test) = checkAll config test

    ///Check all public static methods on the given type that have a testable return type with the given configuration.
    ///This includes let-bound functions in a module.
    static member All<'Test> config = Check.All(config,typeof<'Test>)

    ///Check all public static methods on the given type that have a testable return type with quick configuration
    static member QuickAll test = Check.All(Config.Quick,test)

    ///Check all public static methods on the given type that have a testable return type with quick configuration, 
    ///and throw on failure or exhaustion.
    static member QuickThrowOnFailureAll test = Check.All(Config.QuickThrowOnFailure,test)

    ///Check all public static methods on the given type that have a testable return type with quick configuration
    static member QuickAll<'Test>() = Check.All(Config.Quick,typeof<'Test>)

    ///Check all public static methods on the given type that have a testable return type with quick configuration
    static member QuickThrowOnFailureAll<'Test>() = Check.All(Config.QuickThrowOnFailure,typeof<'Test>)

    /// Check all public static methods on the given type that have a testable return type with vthe erbose configuration
    static member VerboseAll test = Check.All(Config.Verbose,test)

    /// Check all public static methods on the given type that have a testable return type with vthe erbose configuration
    static member VerboseAll<'Test>() = Check.All(Config.Verbose,typeof<'Test>)

    /// Check all public static methods on the given type that have a testable return type with vthe erbose configuration,
    ///and throws on failure or exhaustion.
    static member VerboseThrowOnFailureAll test = Check.All(Config.VerboseThrowOnFailure,test)

    /// Check all public static methods on the given type that have a testable return type with the verbose configuration,
    ///and throws on failure or exhaustion.
    static member VerboseThrowOnFailureAll<'Test>() = Check.All(Config.VerboseThrowOnFailure,typeof<'Test>)


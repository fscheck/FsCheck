namespace FsCheck

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
                * Rnd (*the seed used*)
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

type ParallelRunConfig =
    { // For I/O bound work 1 would be fine, for cpu intensive tasks Environment.ProcessorCount appears to be fastest option
      MaxDegreeOfParallelism : int 
    }

///For configuring a run.
[<NoEquality;NoComparison>]
type Config = 
    { ///The maximum number of tests that are run.
      MaxTest       : int
      ///The maximum number of tests where values are rejected, e.g. as the result of ==>
      MaxFail       : int
      ///If set, the seed to use to start testing. Allows reproduction of previous runs.
      Replay        : Rnd option
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
      Runner        : IRunner
      ///If set, inputs for property generation and property evaluation will be runned in parallel. 
      ParallelRunConfig : ParallelRunConfig option
    }

module Runner =
    open System.Collections.Generic
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
        
    [<NoEquality;NoComparison>]
    type private OutcomeSeqOrFuture =
        | Value of seq<TestStep>
        | Future of Threading.Tasks.Task<seq<TestStep>>
 
    let rec private shrinkResultValue (result :Result) (shrinks :IEnumerator<Rose<Result>>) =
        seq { 
            if (shrinks.MoveNext ()) then
                //result forced here
                let (MkRose ((Lazy result'),shrinks')) = shrinks.Current
                if result'.Outcome.Shrink then yield Shrink result'; yield! shrinkResultValue result' (shrinks'.GetEnumerator())
                else yield NoShrink result'; yield! shrinkResultValue result shrinks
            else yield EndShrink result
        }
 
    let rec private shrinkResultTask (result :Result) (shrinks :IEnumerator<Rose<ResultContainer>>) =
        let goNext (s :seq<Rose<ResultContainer>>) r = 
            async {
                if r.Outcome.Shrink then 
                    let! xs = shrinkResultTask r (s.GetEnumerator ()) |> Async.AwaitTask
                    return seq { yield Shrink r; yield! xs }
                else 
                    let! xs = shrinkResultTask result shrinks |> Async.AwaitTask
                    return seq { yield NoShrink r; yield! xs }
            }
        if (shrinks.MoveNext ()) then
            //result forced here
            let (MkRose ((Lazy result'),shrinks')) = shrinks.Current
            match result' with
            | ResultContainer.Value r -> 
                goNext shrinks' r |> Async.StartAsTask
            | ResultContainer.Future t ->
                let rt = t |> Async.AwaitTask
                async.Bind (rt, goNext shrinks') |> Async.StartAsTask
        else EndShrink result |> Seq.singleton |> Threading.Tasks.Task.FromResult

    let private shrinkResultTaskIter (result: Result) (shrinks :IEnumerator<Rose<ResultContainer>>) =
        let xs :Collections.Generic.List<TestStep> = ResizeArray () 
        let rec iter (result: Result) (shrinks :IEnumerator<Rose<ResultContainer>>) =
            if (shrinks.MoveNext ()) then
                //result forced here
                let (MkRose ((Lazy result'), shrinks')) = shrinks.Current
                match result' with
                | ResultContainer.Value r -> 
                    if r.Outcome.Shrink then 
                        xs.Add <| Shrink r
                        iter r (shrinks'.GetEnumerator ())
                    else
                        xs.Add <| NoShrink r
                        iter result shrinks
                | ResultContainer.Future t ->
                    async {
                        let! rt = t |> Async.AwaitTask
                        if rt.Outcome.Shrink then 
                            let! ys = shrinkResultTask rt (shrinks'.GetEnumerator ()) |> Async.AwaitTask
                            return seq { yield! xs; yield Shrink rt; yield! ys }
                        else 
                            let! ys = shrinkResultTask result shrinks |> Async.AwaitTask
                            return seq { yield! xs; yield NoShrink rt; yield! ys }
                    } |> Async.StartAsTask |> OutcomeSeqOrFuture.Future
            else xs.Add <| EndShrink result; xs :> seq<_> |> OutcomeSeqOrFuture.Value
        iter result shrinks

    let private test initSize resize rnd0 gen =
        let rec test' initSize resize rnd0 ((Gen eval) as gen) =
            seq { 
                let rnd1,rnd2 = Random.split rnd0
                let newSize = resize initSize
                //result forced here!
                let result, shrinks =
                    try
                        let (MkRose (Lazy result, shrinks)) = (eval (newSize |> round |> int) rnd2).Value
                        result, shrinks
                        //printfn "After generate"
                        //problem: since result.Ok is no longer lazy, we only get the generated args _after_ the test is run
                    with :? DiscardException -> 
                        Res.rejectedV, Seq.empty

                yield Generated result.Arguments

                match result.Outcome with
                    | Outcome.Rejected -> 
                        yield Failed result 
                    | Outcome.True -> 
                        yield Passed result
                    | o when o.Shrink -> 
                        yield Falsified result
                        yield! shrinkResultValue result (shrinks.GetEnumerator ())
                    | _ ->
                        yield Falsified result
                        yield EndShrink result
                yield! test' newSize resize rnd1 gen
            }
        //Since we're not runing test for parallel scenarios it's impossible to discover `ResultContainer.Future` inside `result`
        let gen' = gen |> Gen.map (Rose.map (fun rc -> 
                match rc with 
                | ResultContainer.Value r -> r 
                | ResultContainer.Future t -> t.Result))
        test' initSize resize rnd0 gen'

    let private outcomeSeq result (shrinks :seq<_>) = 
        let g = Generated result.Arguments
        match result.Outcome with
        | Outcome.Rejected -> 
            seq {yield g; yield Failed result} |> OutcomeSeqOrFuture.Value
        | Outcome.True -> 
            seq {yield g; yield Passed result} |> OutcomeSeqOrFuture.Value
        | o when o.Shrink -> 
            match shrinkResultTaskIter result (shrinks.GetEnumerator ()) with
            | OutcomeSeqOrFuture.Value v -> seq {yield g; yield Falsified result; yield! v} |> OutcomeSeqOrFuture.Value
            | OutcomeSeqOrFuture.Future t -> t.ContinueWith (fun (xs :Threading.Tasks.Task<seq<TestStep>>) -> 
                seq {yield g; yield Falsified result; yield! xs.Result}) |> OutcomeSeqOrFuture.Future         
        | _ ->
            seq {yield g; yield Falsified result; yield EndShrink result} |> OutcomeSeqOrFuture.Value

    let private testStep rnd (size :float) ((Gen eval) as gen) =
            let result, shrinks =
                try
                    let (MkRose (Lazy result, shrinks)) = (eval (size |> round |> int) rnd).Value
                    result, shrinks
                with :? DiscardException ->
                    Res.rejected, Seq.empty

            match result with
            | ResultContainer.Value r -> outcomeSeq r shrinks
            | ResultContainer.Future t -> t.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> 
                match outcomeSeq x.Result shrinks with
                | OutcomeSeqOrFuture.Value v -> Threading.Tasks.Task.FromResult v
                | OutcomeSeqOrFuture.Future t -> t) |> Threading.Tasks.TaskExtensions.Unwrap |> OutcomeSeqOrFuture.Future

    let stepsSeq resize =
        Seq.unfold (fun (initSize, rnd) ->
            let rnd1, rnd2 = Random.split rnd
            let newSize = resize initSize
            Some ((rnd1, rnd2, newSize), (newSize, rnd1)))

    let private outcomeSeqFutureCont (xs :Threading.Tasks.Task<seq<TestStep>>) (state :obj) =
        match state with 
        | :? (int * Threading.CancellationToken * array<seq<TestStep>> * int) as state ->
            let (j, ct, results, iters) = state
            if (not ct.IsCancellationRequested) && j < iters then
                results.[j] <- xs.Result
        | _ -> raise (System.ArgumentException ("state"))

#if PCL
    let private parallelTest config = test
#else
    let private tpWorkerFun (state :obj) =
        match state with 
        | :? ((Rnd * Rnd * float) array * (int ref) * int * Gen<Rose<ResultContainer>> * array<seq<TestStep>> * Threading.CancellationToken) as state ->
            let (steps, i, iters, gen, results, ct) = state
            let mutable j = 0
            while (not ct.IsCancellationRequested) && j < iters do
                j <- Threading.Interlocked.Increment (i) - 1
                if j < iters then
                    let _, rnd, size = steps.[j]
                    let res = testStep rnd size gen
                    match res with
                    | OutcomeSeqOrFuture.Value xs -> results.[j] <- xs
                    | OutcomeSeqOrFuture.Future ts -> 
                        ts.ContinueWith (outcomeSeqFutureCont, (j, ct, results, iters)) |> ignore                    
        | _ -> raise (System.ArgumentException ("state"))

    ///Enumerates over `steps` seq publishing every item to `tpWorkerFun` via `ThreadPool.QueueUserWorkItem`
    ///Waits for `tpWorkerFun` to populate `results` array
    ///Order is preserved by sitting in busy loop & checking current entry in `results` 
    ///    and yielding processor to other threads if cell haven't been populated yet
    ///To be lean in allocated space first assumes that test would pass and creates `result` of size `maxTest`
    ///    reallocates `result` to hold `maxFail` entries if test is falsified
    ///Other strategies can be considered to trade between allocated memory, overall running time and amount of cross-process communication:
    ///    - publish to `tpWorkerFun` more than one entry at a time
    ///    - allocate `results` in lesser chunks in iterative manner
    ///    - change busy loop with yelding to waiting on conditional variables (tested, leads to slower runing time)
    type private ParSeqEnumerator (steps :IEnumerable<(Rnd * Rnd * float)>, maxTest, maxFail, maxDegreeOfParallelism, gen) =   
        let size = maxTest + maxFail
        let luckySeq = steps |> Seq.take maxTest
        let unluckySeq = steps |> Seq.skip maxTest |> Seq.take maxFail
        let mutable results = Array.zeroCreate<seq<TestStep>> maxTest
        let mutable i = 0
        let indexT = ref 0
        let indexF = ref 0
        let mutable current = Unchecked.defaultof<TestStep>
        let mutable subE = Linq.Enumerable.Empty<TestStep>().GetEnumerator ()
        let mutable cts = new Threading.CancellationTokenSource ()
        let mutable started = false
        let mutable firstRun = true
        let run xs index =
            let xs = Array.ofSeq xs
            for i in 0..(Math.Min (Array.length xs, maxDegreeOfParallelism)) do
                Threading.ThreadPool.QueueUserWorkItem (
                    new Threading.WaitCallback (tpWorkerFun),
                    (xs, index, Array.length xs, gen, results, cts.Token)) |> ignore
        let moveNextInner () = 
            if subE.MoveNext () then
                current <- subE.Current; true  
            else
                subE.Dispose (); false    
        let moveNextOuter () =
            if i = maxTest && firstRun then
                results <- Array.zeroCreate<seq<TestStep>> maxFail
                i <- 0
                run unluckySeq indexF
                firstRun <- false
            while isNull results.[i] do
#if NETSTANDARD1_6
                Threading.Thread.Sleep (1) |> ignore
#else
                Threading.Thread.Yield () |> ignore
#endif
            subE <- results.[i].GetEnumerator ()
            i <- i + 1
        interface IEnumerator<TestStep> with
            member __.MoveNext () = 
                if not started then
                    run luckySeq indexT; started <- true
                let mutable running = true
                let mutable moved = false
                while running do
                    if i >= size then
                        running <- false
                    else
                        if moveNextInner () then
                            running <- false
                            moved <- true
                        else
                            moveNextOuter ()
                moved
            member __.Current :TestStep = current
            member __.Current :obj = box current
            member __.Reset () = 
                cts.Cancel ()
                cts.Dispose ()
                cts <- new Threading.CancellationTokenSource ()
                started <- false
                firstRun <- true
                results <- Array.zeroCreate<seq<TestStep>> maxTest
                current <- Unchecked.defaultof<TestStep>
                i <- 0
                indexT := 0
                indexF := 0
                subE.Dispose ()
        interface IDisposable with
            member __.Dispose () = 
                cts.Cancel ()
                subE.Dispose ()
                cts.Dispose ()


    type private ParTestSeq (steps, maxTest, maxFail, maxDegreeOfParallelism, gen) =   
        let enumerator () = new ParSeqEnumerator (steps, maxTest, maxFail, maxDegreeOfParallelism, gen)
        interface IEnumerable<TestStep> with
            member __.GetEnumerator () = enumerator () :> IEnumerator<TestStep>
            member __.GetEnumerator () = enumerator () :> System.Collections.IEnumerator

    let private parallelTest config initSize resize rnd0 gen =
        let steps = stepsSeq resize (initSize, rnd0)
        let pd = Option.fold (fun _ pc -> if pc.MaxDegreeOfParallelism <> -1 then pc.MaxDegreeOfParallelism else Environment.ProcessorCount) 1 config.ParallelRunConfig
        let parSeq = ParTestSeq (steps, config.MaxTest, config.MaxFail, pd, gen)
        parSeq :> seq<_>
#endif


    let private testsDone config testStep origArgs ntest nshrinks usedSeed stamps  =
        let entry (n,xs) = (100 * n / ntest),xs
        let table = stamps 
                    |> Seq.filter (not << List.isEmpty)
                    |> Seq.sort
                    |> Seq.groupBy id
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
        let lastStep = ref (Failed Res.rejectedV)
        let seed = match config.Replay with None -> Random.create() | Some s -> s
        let increaseSizeStep = float (config.EndSize - config.StartSize) / float config.MaxTest
        let testSeq = 
            if config.ParallelRunConfig.IsSome then 
                parallelTest config 
             else 
                test
        testSeq (float config.StartSize) ((+) increaseSizeStep) seed (property prop |> Property.GetGen)
        |> Common.takeWhilePlusLast (fun step ->
            lastStep := step
            match step with
                | Generated args -> config.Runner.OnArguments(!testNb, args, config.Every); true
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

    let private labelsToString l = String.concat newline l

    let private maybePrintLabels (l:Set<_>) = 
        match l.Count with
        | 0 -> String.Empty
        | 1 -> sprintf "Label of failing property: %s%s" (labelsToString l) newline
        | _ -> sprintf "Labels of failing property (one or more is failing):%s%s%s" newline (labelsToString l) newline

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
        let stampsToString s = s |> Seq.map entry |> Seq.toList |> display
        let name = if String.IsNullOrEmpty(name) then String.Empty else (name+"-")
        match testResult with
        | TestResult.True (data, suppressOutput) ->
            if suppressOutput then ""
            else sprintf "%sOk, passed %i test%s%s"
                    name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stampsToString)
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
                name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stampsToString )

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
    [<Obsolete("Calling this should no longer be necessary - though please file an issue if you find a case where it is.")>]
    let init = 
        Arb.defaultArbitrary |> ignore
        lazy ()

    let private hasTestableReturnType (m:MethodInfo) =
        try
            testableTC.GetInstance m.ReturnType |> ignore
            true
        with
            _ -> false

    let internal check config p = 
        //save so we can restore after the run
        let defaultArbitrary = Arb.arbitrary.Value
        let merge newT (existingTC:TypeClass<_>) = existingTC.DiscoverAndMerge(onlyPublic=true,instancesType=newT)
        Arb.arbitrary.Value <- List.foldBack merge config.Arbitrary defaultArbitrary
        try
            runner config (property p)
        finally
            Arb.arbitrary.Value <- defaultArbitrary

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
        | [||] -> [||]
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
              Runner        = consoleRunner
              ParallelRunConfig = None } 

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


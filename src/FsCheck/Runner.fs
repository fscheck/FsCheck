namespace FsCheck

open System

open FsCheck.FSharp
open FsCheck.Internals

[<NoEquality; NoComparison>]
type TestData =
    { NumberOfTests: int
      NumberOfShrinks: int
      Stamps: seq<int * list<string>>
      Labels: Set<string> }

[<NoEquality;NoComparison;RequireQualifiedAccess>]
type TestResult = 
    | Passed of TestData
                * bool(*suppress output*)
    | Failed of TestData 
                * list<obj>(*the original arguments that produced the failed test*)
                * list<obj>(*the shrunk arguments that produce a failed test*)
                * Outcome(*possibly exception or timeout that falsified the property*) 
                * Rnd (*initial seed for test run*)
                * Rnd (*the seed used for generation of arguments that falsified test*)
                * int (*the size used for generation of arguments that falsified test*)
    | Exhausted of TestData

///For implementing your own test runner.
type IRunner =
    ///Called before a group of properties on a type are checked.
    abstract OnStartFixture: System.Type -> unit
    ///Called whenever arguments are generated and after the test is run.
    abstract OnArguments: int * list<obj> * (int -> list<obj> -> string) -> unit
    ///Called on a successful shrink.
    abstract OnShrink: list<obj> * (list<obj> -> string) -> unit
    ///Called whenever all tests are done, either Passed, Failed or Exhausted.
    abstract OnFinished: string * TestResult -> unit

type Replay = { Rnd: Rnd; Size: int option }

type ParallelRunConfig =
    { /// For I/O bound work 1 would be fine, for cpu intensive tasks Environment.ProcessorCount appears to be fastest option
      MaxDegreeOfParallelism: int }

///For configuring a run.
[<NoEquality;NoComparison>]
type Config = private Config of
                {|
                MaxTest           : int
                MaxRejected       : int
                Replay            : Replay option
                Name              : string
                StartSize         : int
                EndSize           : int
                QuietOnSuccess    : bool
                Every             : int -> list<obj> -> string
                EveryShrink       : list<obj> -> string 
                ArbMap            : IArbMap
                Runner            : IRunner 
                ParallelRunConfig : ParallelRunConfig option
                |} with
                member private this.Values =
                    let values (Config config) = config
                    this |> values
                ///The maximum number of tests that are run.
                member this.MaxTest = this.Values.MaxTest
                ///The maximum number of tests where values are rejected, e.g. as the result of ==>
                member this.MaxRejected = this.Values.MaxRejected
                ///If set, the seed to use to start testing. Allows reproduction of previous runs.
                member this.Replay = this.Values.Replay
                ///Name of the test.
                member this.Name = this.Values.Name
                ///The size to use for the first test.
                member this.StartSize = this.Values.StartSize
                ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
                member this.EndSize = this.Values.EndSize
                ///If set, suppresses the output from the test if the test is successful.
                member this.QuietOnSuccess = this.Values.QuietOnSuccess
                ///What to print when new arguments args are generated in test n
                member this.Every = this.Values.Every
                ///What to print every time a counter-example is successfully shrunk
                member this.EveryShrink = this.Values.EveryShrink
                /////The Arbitrary instances on this class will be merged in back to front order, i.e. instances for the same generated type at the front
                /////of the list will override those at the back. The instances on Arb.Default are always known, and are at the back (so they can always be
                /////overridden)
                //member this.Arbitrary = this.Values.Arbitrary
                member this.ArbMap = this.Values.ArbMap
                ///A custom test runner, e.g. to integrate with a test framework like xUnit or NUnit. 
                member this.Runner = this.Values.Runner
                ///If set, inputs for property generation and property evaluation will be run in parallel. 
                member this.ParallelRunConfig = this.Values.ParallelRunConfig
                ///Returns a new Config with specified MaxTest
                member this.WithMaxTest maxTest =
                    {|this.Values with MaxTest = maxTest|} |> Config
                ///Returns a new Config with specified MaxRejected
                member this.WithMaxRejected maxRejected =
                    {|this.Values with MaxRejected = maxRejected|} |> Config
                ///Returns a new Config with specified Replay option
                member this.WithReplay replay =
                    {|this.Values with Replay = replay|} |> Config  
                ///Returns a new Config with specified Name
                member this.WithName name =
                    {|this.Values with Name = name|} |> Config                
                ///Returns a new Config with specified StartSize
                member this.WithStartSize startSize =
                    {|this.Values with StartSize = startSize|} |> Config
                ///Returns a new Config with specified EndSize
                member this.WithEndSize endSize =
                    {|this.Values with EndSize = endSize|} |> Config
                ///Returns a new Config with specified QuietOnSuccess
                member this.WithQuietOnSuccess quietOnSuccess =
                    {|this.Values with QuietOnSuccess = quietOnSuccess|} |> Config
                ///Returns a new Config with specified Every function
                member this.WithEvery every =
                    {|this.Values with Every = every|} |> Config
                ///Returns a new Config with specified EveryShrink function
                member this.WithEveryShrink everyShrink =
                    {|this.Values with EveryShrink = everyShrink|} |> Config
                ///Returns a new Config with specified Arbitrary
                member this.WithArbitrary arbitrary =
                    let result = this.ArbMap |> Seq.foldBack ArbMap.mergeWithType arbitrary
                    {|this.Values with ArbMap = result|} |> Config
                ///Returns a new Config with specified Runner
                member this.WithRunner runner =
                    {|this.Values with Runner = runner|} |> Config
                ///Returns a new Config with specified ParallelRunConfig
                member this.WithParallelRunConfig config =
                    {|this.Values with ParallelRunConfig = config|} |> Config
                
                    

                    

module Runner =
    open System.Collections.Generic
    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Testable
    open FsCheck.Internals.TypeClass
   
    [<NoEquality;NoComparison>]
    type private TestStep = 
        | Run of Result * int * Rnd
        | Shrink of Result
        | Stop

        
    [<NoEquality;NoComparison>]
    type private OutcomeSeqOrFuture =
        | Value of seq<TestStep>
        | Future of Threading.Tasks.Task<seq<TestStep>>
 
    let rec private shrinkResultValue (result :Result) (shrinks :IEnumerator<Shrink<Result>>) =
        seq { 
            if (shrinks.MoveNext ()) then
                //result forced here
                let (result',shrinks') = shrinks.Current |> Shrink.getValue
                yield Shrink result'
                if result'.Outcome.Shrink then
                    yield! shrinkResultValue result' (shrinks'.GetEnumerator())
                else
                    yield! shrinkResultValue result shrinks
            else
                yield Stop
        }
 
    let rec private shrinkResultTask (result :Result) (shrinks :IEnumerator<Shrink<ResultContainer>>) =
        let goNext (s :seq<Shrink<ResultContainer>>) r = 
            async {
                if r.Outcome.Shrink then 
                    let! xs = shrinkResultTask r (s.GetEnumerator ()) |> Async.AwaitTask
                    return seq { yield Shrink r; yield! xs }
                else 
                    let! xs = shrinkResultTask result shrinks |> Async.AwaitTask
                    return seq { yield Shrink r; yield! xs }
            }
        if (shrinks.MoveNext ()) then
            //result forced here
            let (result',shrinks') = shrinks.Current |> Shrink.getValue
            match result' with
            | ResultContainer.Value r -> 
                goNext shrinks' r |> Async.StartAsTask
            | ResultContainer.Future t ->
                let rt = t |> Async.AwaitTask
                async.Bind (rt, goNext shrinks') |> Async.StartAsTask
        else Stop |> Seq.singleton |> Threading.Tasks.Task.FromResult

    let private shrinkResultTaskIter (result: Result) (shrinks :IEnumerator<Shrink<ResultContainer>>) =
        let xs :List<TestStep> = ResizeArray () 
        let rec iter (result: Result) (shrinks :IEnumerator<Shrink<ResultContainer>>) =
            if (shrinks.MoveNext ()) then
                //result forced here
                let (result', shrinks') = shrinks.Current |> Shrink.getValue
                match result' with
                | ResultContainer.Value r -> 
                    if r.Outcome.Shrink then 
                        xs.Add <| Shrink r
                        iter r (shrinks'.GetEnumerator ())
                    else
                        xs.Add <| Shrink r
                        iter result shrinks
                | ResultContainer.Future t ->
                    async {
                        let! rt = t |> Async.AwaitTask
                        if rt.Outcome.Shrink then 
                            let! ys = shrinkResultTask rt (shrinks'.GetEnumerator ()) |> Async.AwaitTask
                            return seq { yield! xs; yield Shrink rt; yield! ys }
                        else 
                            let! ys = shrinkResultTask result shrinks |> Async.AwaitTask
                            return seq { yield! xs; yield Shrink rt; yield! ys }
                    } |> Async.StartAsTask |> OutcomeSeqOrFuture.Future
            else
                xs.Add <| Stop
                xs :> seq<_> |> OutcomeSeqOrFuture.Value
        iter result shrinks

    let stepsSeq resize =
        Seq.unfold (fun (initSize, rnd) ->
            let rnd1, rnd2 = Random.Split rnd
            let newSize = resize initSize
            Some ((newSize, rnd2), (newSize, rnd1)))

    let private testSingle generator (newSize, seed) =
        let size = newSize |> round |> int
        let result, shrinks =
            try
                generator
                |> Gen.run size seed  
                |> Shrink.getValue
            with :? DiscardException -> 
                Res.rejectedV, Seq.empty
        
        seq {
            yield Run (result, size, seed)
            if result.Outcome.Shrink then
                yield! shrinkResultValue result (shrinks.GetEnumerator ())
        }

    let private test isReplay (initSize:float) resize rnd0 gen =    
        //Since we're not running test for parallel scenarios it's impossible to discover `ResultContainer.Future` inside `result`
        let gen' = gen |> Gen.map (Shrink.map (fun rc -> 
                match rc with 
                | ResultContainer.Value r -> r 
                | ResultContainer.Future t -> t.Result))
        if isReplay then
            testSingle gen' (initSize, rnd0)
        else
            let source = stepsSeq resize (initSize, rnd0)
            Seq.collect (testSingle gen') source

    let private outcomeSeq result (shrinks :seq<_>) rnd usedSize = 
        //let data = {| Args = result.Arguments, rnd, usedSize)
        let runResult = Run (result, usedSize, rnd)
        match result.Outcome with
        | o when o.Shrink -> 
            match shrinkResultTaskIter result (shrinks.GetEnumerator ()) with
            | OutcomeSeqOrFuture.Value v -> seq { yield runResult; yield! v} |> OutcomeSeqOrFuture.Value
            | OutcomeSeqOrFuture.Future t -> t.ContinueWith (fun (xs :Threading.Tasks.Task<seq<TestStep>>) -> 
                seq {yield runResult; yield! xs.Result}) |> OutcomeSeqOrFuture.Future         
        | _ ->
            seq {yield runResult} |> OutcomeSeqOrFuture.Value

    let private testStep rnd (size :float) gen =
            let usedSize = size |> round |> int
            let result, shrinks =
                try
                    gen
                    |> Gen.run usedSize rnd
                    |> Shrink.getValue
                with :? DiscardException ->
                    Res.rejected, Seq.empty

            match result with
            | ResultContainer.Value r -> outcomeSeq r shrinks rnd usedSize
            | ResultContainer.Future t -> t.ContinueWith (fun (x :Threading.Tasks.Task<Result>) -> 
                match outcomeSeq x.Result shrinks rnd usedSize with
                | OutcomeSeqOrFuture.Value v -> Threading.Tasks.Task.FromResult v
                | OutcomeSeqOrFuture.Future t -> t) |> Threading.Tasks.TaskExtensions.Unwrap |> OutcomeSeqOrFuture.Future

    let private outcomeSeqFutureCont (xs :Threading.Tasks.Task<seq<TestStep>>) (state :obj) =
        match state with 
        | :? (int * Threading.CancellationToken * array<seq<TestStep>> * int) as state ->
            let (j, ct, results, iters) = state
            if (not ct.IsCancellationRequested) && j < iters then
                results.[j] <- xs.Result
        | _ -> raise (ArgumentException ("state"))

    let private tpWorkerFun (state :obj) =
        match state with 
        | :? ((float * Rnd) array * (int ref) * int * Gen<Shrink<ResultContainer>> * array<seq<TestStep>> * Threading.CancellationToken) as state ->
            let (steps, i, iters, gen, results, ct) = state
            //let oldValue = Arb.arbitrary.Value
            //try
                //Arb.arbitrary.Value <- defaultArb
            let mutable j = 0
            while (not ct.IsCancellationRequested) && j < iters do
                j <- Threading.Interlocked.Increment (i) - 1
                if j < iters then
                    let rnd, size = steps.[j]
                    let res = testStep size rnd gen
                    match res with
                    | OutcomeSeqOrFuture.Value xs -> results.[j] <- xs
                    | OutcomeSeqOrFuture.Future ts -> 
                        ts.ContinueWith (outcomeSeqFutureCont, (j, ct, results, iters)) |> ignore
            //finally
                //Arb.arbitrary.Value <- oldValue
            
        | _ -> invalidArg "state" (sprintf "This is a bug in FsCheck, please report it. Unexpected argument: %O" state)

    ///Enumerates over `steps` seq publishing every item to `tpWorkerFun` via `ThreadPool.QueueUserWorkItem`
    ///Waits for `tpWorkerFun` to populate `results` array
    ///Order is preserved by sitting in busy loop & checking current entry in `results` 
    ///    and yielding processor to other threads if cell haven't been populated yet
    ///To be lean in allocated space first assumes that test would pass and creates `result` of size `maxTest`
    ///    reallocates `result` to hold `maxFail` entries if test is falsified
    ///Other strategies can be considered to trade between allocated memory, overall running time and amount of cross-process communication:
    ///    - publish to `tpWorkerFun` more than one entry at a time
    ///    - allocate `results` in lesser chunks in iterative manner
    ///    - change busy loop with yielding to waiting on conditional variables (tested, leads to slower running time)
    type private ParSeqEnumerator (steps :IEnumerable<(float * Rnd)>, maxTest, maxReject, maxDegreeOfParallelism, gen) =   
        let size = maxTest + maxReject
        let luckySeq = steps |> Seq.take maxTest
        let unluckySeq = steps |> Seq.skip maxTest |> Seq.take maxReject
        let mutable results = Array.zeroCreate<seq<TestStep>> maxTest
        let mutable i = 0
        let indexT = ref 0
        let indexF = ref 0
        let mutable current = Unchecked.defaultof<TestStep>
        let mutable subE = Seq.empty<TestStep>.GetEnumerator ()
        let mutable cts = new Threading.CancellationTokenSource ()
        let mutable started = false
        let mutable firstRun = true
        let run xs index =
            let xs = Array.ofSeq xs
            for i in 0..(Math.Min (Array.length xs, maxDegreeOfParallelism)) do
                Threading.ThreadPool.QueueUserWorkItem (
                    new Threading.WaitCallback (tpWorkerFun),
                    (xs, index, Array.length xs, gen, results, cts.Token)) |> ignore
        let moveNextInner() = 
            if subE.MoveNext() then
                current <- subE.Current
                true  
            else
                subE.Dispose()
                false    
        let moveNextOuter() =
            if i = maxTest && firstRun then
                results <- Array.zeroCreate<seq<TestStep>> maxReject
                i <- 0
                run unluckySeq indexF
                firstRun <- false
            while isNull results.[i] do
                Threading.Thread.Yield () |> ignore
            subE <- results.[i].GetEnumerator()
            i <- i + 1
        interface IEnumerator<TestStep> with
            member __.MoveNext () = 
                if not started then
                    run luckySeq indexT
                    started <- true
                let mutable running = true
                let mutable moved = false
                while running do
                    if i >= size then
                        running <- false
                    else
                        if moveNextInner() then
                            running <- false
                            moved <- true
                        else
                            moveNextOuter()
                moved
            member __.Current :TestStep = current
            member __.Current :obj = box current
            member __.Reset () = 
               raise <| NotSupportedException("Reset on ParSeqEnumerator is not supported")

        interface IDisposable with
            member __.Dispose() = 
                cts.Cancel()
                subE.Dispose()
                cts.Dispose()


    type private ParTestSeq (steps, maxTest, maxFail, maxDegreeOfParallelism, gen) =   
        let enumerator () = new ParSeqEnumerator (steps, maxTest, maxFail, maxDegreeOfParallelism, gen)
        interface IEnumerable<TestStep> with
            member __.GetEnumerator () = enumerator () :> IEnumerator<TestStep>
            member __.GetEnumerator () = enumerator () :> System.Collections.IEnumerator

    let private parallelTest (config:Config) initSize resize rnd0 gen =
        let steps = stepsSeq resize (initSize, rnd0)
        let pd = Option.fold (fun _ pc -> if pc.MaxDegreeOfParallelism <> -1 then pc.MaxDegreeOfParallelism else Environment.ProcessorCount) 1 config.ParallelRunConfig
        let parSeq = ParTestSeq (steps, config.MaxTest, config.MaxRejected, pd, gen)
        parSeq :> seq<_>


    let private testsDone (config:Config) testStep origArgs ntest nshrinks originalSeed lastSeed lastSize stamps =
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
                | Run ({ Outcome = Outcome.Passed },_,_) -> TestResult.Passed (testData, config.QuietOnSuccess)
                | Run ({ Outcome = Outcome.Failed _ } as result,_,_) -> TestResult.Failed ({ testData with Labels=result.Labels }, origArgs, result.Arguments, result.Outcome, originalSeed, lastSeed, lastSize)
                | Run ({ Outcome = Outcome.Rejected },_,_) -> TestResult.Exhausted testData
                | Shrink result -> TestResult.Failed ({ testData with Labels=result.Labels }, origArgs, result.Arguments, result.Outcome, originalSeed, lastSeed, lastSize)
                | Stop -> invalidArg "testStep" "Cannot be Stop."
        config.Runner.OnFinished(config.Name,testResult)


    let private runner (config:Config) prop = 
        let testNb = ref 0
        let rejectedNb = ref 0
        let shrinkNb = ref 0
        let tryShrinkNb = ref 0
        let origArgs = ref []
        let lastStep = ref (Run (Res.rejectedV,-1,Rnd()))
        let seed, size = match config.Replay with None -> Random.Create(), None | Some s -> s.Rnd, s.Size
        let increaseSizeStep = float (config.EndSize - config.StartSize) / float config.MaxTest
        let lastSeed = ref seed
        let lastSize = ref (defaultArg size config.StartSize)
        let testSeq = 
            if config.ParallelRunConfig.IsSome && size.IsNone then //no point to run single test in parallel
                parallelTest config 
            else 
                test size.IsSome
        testSeq (float <| defaultArg size config.StartSize) ((+) increaseSizeStep) seed (property prop |> Property.GetGen config.ArbMap)
        |> Seq.takeWhile (fun step ->
            match step with
                | Run (result,s,seed) ->
                    lastStep := step
                    lastSeed := seed
                    lastSize := s
                    config.Runner.OnArguments(!testNb, result.Arguments, config.Every)
                    match result with
                    | { Outcome = Outcome.Passed } -> 
                        testNb := !testNb + 1
                        !testNb <> config.MaxTest && size.IsNone //stop if we have enough tests or this was fast-forward single test run
                    | { Outcome = Outcome.Failed _ } -> 
                        origArgs := result.Arguments
                        testNb := !testNb + 1
                        true //failed, true to continue with shrinking
                    | { Outcome = Outcome.Rejected } -> 
                        rejectedNb := !rejectedNb + 1
                        !rejectedNb <> config.MaxRejected //rejected, stop if we have too much failed tests
                | Shrink ({ Outcome = Outcome.Failed _ } as result) -> 
                    lastStep := step
                    tryShrinkNb := 0
                    shrinkNb := !shrinkNb + 1
                    config.Runner.OnShrink(result.Arguments, config.EveryShrink)
                    true
                | Shrink _ ->
                    lastStep := step
                    tryShrinkNb := !tryShrinkNb + 1
                    true
                | Stop -> 
                    false)
        |> Seq.fold (fun acc elem ->
            match elem with
                | Run ({ Outcome = Outcome.Passed } as result,_,_) -> (result.Stamp :: acc)
                | _ -> acc
            ) [] 
        |> testsDone config !lastStep !origArgs !testNb !shrinkNb seed !lastSeed !lastSize

    let private newline = Environment.NewLine

    let argumentsToString args =
        let escapeControlChars (s:string) =
            if isNull s then s
            else
                let result = System.Text.StringBuilder()
                let mutable escaped = false
                s |> String.iter (fun ch ->
                        if not (Char.IsControl(ch)) || ch = '\n' || ch = '\r' || ch = '\t' then
                            result.Append(ch) |> ignore
                        else
                            escaped <- true
                            result.Append(ch |> int |> sprintf "\%03i") |> ignore)
                if escaped then result.Append(" (At least one control character has been escaped as a char code, e.g. \\023)") |> ignore
                result.ToString()

        args
        |> List.map (sprintf "%A" >> escapeControlChars)
        |> String.concat newline

    let onStartFixtureToString (t:Type) =
        sprintf "--- Checking %s ---%s" t.Name newline

    let private pluralize nb = if nb = 1 then String.Empty else "s"

    let private labelsToString l = String.concat newline l

    let private maybePrintLabels (l:Set<_>) = 
        match l.Count with
        | 0 -> String.Empty
        | 1 -> sprintf "Label of failing property: %s%s" (labelsToString l) newline
        | _ -> sprintf "Labels of failing property (one or more is failing):%s%s%s" newline (labelsToString l) newline

    let onFailureToString name data originalArgs args usedSeed lastSeed lastSize =
        sprintf "%sFalsifiable, after %i test%s (%i shrink%s) (%A)%sLast step was invoked with size of %i and seed of (%A):%s" 
                name data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed newline lastSize lastSeed newline
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
        | TestResult.Passed (data, suppressOutput) ->
            if suppressOutput then ""
            else sprintf "%sOk, passed %i test%s%s"
                    name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stampsToString)
        | TestResult.Failed (data, originalArgs, args, Outcome.Failed exc, originalSeed, lastSeed, lastSize) -> 
            onFailureToString name data originalArgs args originalSeed lastSeed lastSize
            + sprintf "with exception:%s%O%s" newline exc newline
        | TestResult.Failed (data, originalArgs, args, _, originalSeed, lastSeed, lastSize) -> 
            onFailureToString name data originalArgs args originalSeed lastSeed lastSize
        | TestResult.Exhausted data -> 
            sprintf "%sArguments exhausted after %i test%s%s" 
                name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stampsToString )

    let onArgumentsToString n args = 
        sprintf "%i:%s%s%s" n newline (argumentsToString args) newline
    
    let onShrinkToString args =
        sprintf "shrink:%s%s%s" newline (argumentsToString args) newline
    
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

    let private hasTestableReturnType (m:MethodInfo) =
        try
            testableTC.GetInstance m.ReturnType |> ignore
            true
        with
            _ -> false

    let internal check (config:Config) p = 
        //save so we can restore after the run
        //let defaultArbitrary = Arb.arbitrary.Value
        //let merge newT (existingTC:TypeClass<_>) = existingTC.DiscoverAndMerge(onlyPublic=true,instancesType=newT)
        //Arb.arbitrary.Value <- List.foldBack merge config.Arbitrary defaultArbitrary
        //try
            runner config (property p)
        //finally
            //Arb.arbitrary.Value <- defaultArbitrary

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
        

    let internal checkAll (config:Config) (t:Type) = 
        config.Runner.OnStartFixture t
        t.GetRuntimeMethods() 
        |> Seq.filter (fun meth -> meth.IsStatic && meth.IsPublic)
        |> Seq.filter hasTestableReturnType 
        |> Seq.iter (fun m -> checkMethod (config.WithName(t.Name+"."+m.Name))  m None)
        printf "%s" newline

open Runner

type Config with
    ///The quick configuration only prints a summary result at the end of the test.
    static member Quick =
            {| MaxTest       = 100
               MaxRejected   = 1000
               Replay        = None
               Name          = ""
               StartSize     = 1
               EndSize       = 100
               QuietOnSuccess = false
               Every         = fun _ _ -> String.Empty
               EveryShrink   = fun _ -> String.Empty
               ArbMap        = ArbMap.defaults
               Runner        = consoleRunner
               ParallelRunConfig = None |} |> Config 

    ///The verbose configuration prints each generated argument.
    static member Verbose = 
        Config.Quick
                .WithEvery(fun n args -> sprintf "%i:%s%s%s" n Environment.NewLine (argumentsToString args) Environment.NewLine)
                .WithEveryShrink(fun args -> sprintf "shrink:%s%s%s" Environment.NewLine (argumentsToString args) Environment.NewLine)

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
                | TestResult.Passed _ -> printf "%s" (onFinishedToString name testResult)
                | _ -> failwithf "%s" (onFinishedToString name testResult)

        }

    ///Like the Quick configuration, only throws an exception with the error message if the test fails or is exhausted.
    ///Useful for use within other unit testing frameworks that usually adopt this methodology to signal failure.
    static member QuickThrowOnFailure =
        Config.Quick.WithRunner(Config.throwingRunner)

    ///Like the Verbose configuration, only throws an exception with the error message if the test fails or is exhausted.
    ///Useful for use within other unit testing frameworks that usually adopt this methodology to signal failure.
    static member VerboseThrowOnFailure =
        Config.Verbose.WithRunner(Config.throwingRunner)

    ///The default configuration is the quick configuration.
    static member Default = Config.Quick

[<AbstractClass;Sealed>]
type Check =

    ///Check the given property using the given config.
    static member One (config,property:'Testable) = check config property

    ///Check the given property using the given config, and the given test name.
    static member One (name,(config:Config),property:'Testable) = check (config.WithName(name)) property
    
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

    /// Check all public static methods on the given type that have a testable return type with the verbose configuration
    static member VerboseAll test = Check.All(Config.Verbose,test)

    /// Check all public static methods on the given type that have a testable return type with the verbose configuration
    static member VerboseAll<'Test>() = Check.All(Config.Verbose,typeof<'Test>)

    /// Check all public static methods on the given type that have a testable return type with the verbose configuration,
    ///and throws on failure or exhaustion.
    static member VerboseThrowOnFailureAll test = Check.All(Config.VerboseThrowOnFailure,test)

    /// Check all public static methods on the given type that have a testable return type with the verbose configuration,
    ///and throws on failure or exhaustion.
    static member VerboseThrowOnFailureAll<'Test>() = Check.All(Config.VerboseThrowOnFailure,typeof<'Test>)


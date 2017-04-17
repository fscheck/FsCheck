namespace FsCheck.Experimental.Test

module StateMachine =

    open Xunit
    open FsCheck
    open FsCheck.Experimental
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Swensen.Unquote
    open FsCheck.Xunit

    type SimpleModel(?init) =
        let count = ref (defaultArg init 0)
        member __.Inc() = incr count
        member __.Add i = count := !count + i
        member __.Get = !count

    let checkSimpleModelSpec size =
        let inc = StateMachine.operation "inc" ((+) 1) (fun (actual:SimpleModel,model) -> actual.Get = model)
        let create = StateMachine.setup (fun () -> SimpleModel()) (fun () -> 0)
        { new Machine<_,_>(size) with
            member __.Setup = Gen.constant create |> Arb.fromGen
            member __.Next _ = Gen.constant inc }

    [<Fact>]
    let ``check generated commands``() =
        let commands =
            StateMachine.generate (checkSimpleModelSpec -1)
            |> Gen.sample 10 10

        for { Setup = _,create; Operations = comms } in commands do
            typeof<SimpleModel> =! create.Actual().GetType()
            0 =! create.Model()
            for comm,_ in comms do
                test <@ comm.Pre 5 @>
                6 =! comm.Run 5


    type FaultyInc(n:int) =
        inherit Operation<SimpleModel,int>()
        member __.N = n
        override __.Check (a, m) = 
            a.Add n
            if m > 2 then false.ToProperty() else true.ToProperty()
        override __.Run m = m + n
        override __.ToString() = "faultyInc"

    type FaultyCmd = 
        static member Arb = 
            // make sure that minimum counterexample cannot be found with generator .. therefore 5
            let generator = Gen.constant (new FaultyInc(5) :> Operation<SimpleModel,int>)
            // should be able to find minimum counterexample
            let shrinker (op:Operation<SimpleModel,int>) = 
                seq { for i in 1 .. (op :?> FaultyInc).N - 1 do yield FaultyInc i :> Operation<SimpleModel,int>} 
            Arb.fromGenShrink(generator,shrinker)


    let checkFaultyCommandModalSpecWithSetupShrink create =
        { new Machine<_,_>() with
            member __.Setup =
                Arb.fromGenShrink(
                    Gen.choose (50,100) |> Gen.map create,
                    fun setup -> seq { if setup.Model() > 0 then yield create (setup.Model()-1) })
            member __.Next _ =
                FaultyCmd.Arb.Generator }

    [<Fact>]
    let ``should shrink operations``() =
        let create init = StateMachine.setup (fun () -> SimpleModel(init)) (fun () -> init)
        let spec = checkFaultyCommandModalSpecWithSetupShrink create
        let run = { Setup = (53,create 53)
                    TearDown = spec.TearDown
                    Operations = [(new FaultyInc(1) :> Operation<SimpleModel,int>,54)]
                    UsedSize = 1 }
        let shrunk = StateMachine.shrink spec run |> Seq.toArray
        test <@ 1 = shrunk.Length @>
        let run = shrunk.[0]
        test <@ fst run.Setup = 52 @>
        test <@ (snd run.Setup).Model() = 52  @>
        test <@ snd run.Operations.Head = 53 @>
        
    let checkFaultyCommandModelSpec size =
        let create = StateMachine.setup (fun () -> SimpleModel()) (fun () -> 0)
        { new Machine<_,_>(size) with
            member __.Setup = Gen.constant create |> Arb.fromGen
            member __.Next _ = FaultyCmd.Arb.Generator }

    [<Fact>]
    let ``should check faulty command spec and find minimum counterexample``() =
        Arb.register<FaultyCmd>() |> ignore
        let create = StateMachine.setup (fun () -> SimpleModel()) (fun () -> 0)
        let spec = checkFaultyCommandModelSpec -1
        let run = { Setup = (0,create)
                    TearDown = spec.TearDown
                    Operations = [(new FaultyInc(5) :> Operation<SimpleModel,int>,5)]
                    UsedSize = 1 }
        //should contain an element smaller than 5 since they can't be generated but only shrunk
        let shrunk = StateMachine.shrink spec run
        test <@ shrunk |> Seq.exists (fun e -> (e.Operations.Head |> fst :?> FaultyInc).N < 5 ) @>


    //only for specs with no preconditions
    //if a precondition is not found in time None is returned and the length would be 0
    [<Fact>] 
    let ``without stop command length of commands should be Machine.MaxNumberOfCommands``() =
        let specWithLength = checkSimpleModelSpec 3
        let runs = StateMachine.generate specWithLength |> Gen.sample 100 10
        test <@ runs |> Seq.forall (fun { Operations = cmds } -> cmds.Length = 3) @>

    //every tenth command is a stop
    //used to see if it can terminate before reaching the MaxNumberOfCommands 
    let checkStoppingSpec size =
        let inc = StateMachine.operation "inc" ((+) 1) (fun (actual:SimpleModel,model) -> actual.Get = model)
        let stop = new StopOperation<SimpleModel,int>() :> Operation<SimpleModel,int>
        let create = StateMachine.setup (fun () -> SimpleModel()) (fun () -> 0)
        { new Machine<_,_>(size) with
            member __.Setup = Gen.constant create |> Arb.fromGen
            member __.Next _ = Gen.frequency [(9, inc |> Gen.constant);(1, stop |> Gen.constant)] }

    [<Fact>] 
    let ``stop command can terminate before MaxNumberOfCommands is reached``() =
        let specWithLength = checkStoppingSpec 10
        let runs = StateMachine.generate specWithLength |> Gen.sample 100 20
        let len = List.fold (fun acc { Operations = cmds } -> acc + cmds.Length) 0 runs
        test <@ len > 0 && len < 200 @>

    //this spec is created using preconditions such that the only valid sequence is setFalse,setTrue
    //repeated 0 or more times. To simplify the test it doesn't even use an actual object under test;
    //the specification is just meant to check that preconditions are handled correctly.
    let checkPreconditionSpec =
        let setTrue = 
            StateMachine.operationWithPrecondition "setTrue" not (fun _ -> true) (fun (_, _) -> true)
        let setFalse = 
            StateMachine.operationWithPrecondition "setFalse" id (fun _ -> false) (fun (_, _) -> true)
        let create =
            StateMachine.setup id (fun () -> true)
        { new Machine<_,_>() with
            member __.Setup = Gen.constant create |> Arb.fromGen
            member __.Next _ = Gen.elements [setTrue; setFalse] }


    let inline checkPreconditions initial (cmds:seq<Operation<_,_> * _>) =
        cmds
        |> Seq.fold (fun (model,pres) (cmd,_) -> cmd.Run model,pres && cmd.Pre model) (initial, true)
        |> snd

    [<Fact>]
    let ``generate commands should never violate precondition``() =
        test <@ StateMachine.generate checkPreconditionSpec
                |> Gen.sample 100 10
                |> Seq.forall (fun { Setup = _,c; Operations = cmds } -> checkPreconditions (c.Model()) cmds) @>

    [<Fact>]
    let ``shrink commands should never violate precondition``() =
        let counterexample = 
                StateMachine.generate checkPreconditionSpec 
                |> Gen.sample 100 10
                |> Seq.collect (StateMachine.shrink checkPreconditionSpec)
                |> Seq.tryFind (fun { Setup = _,c; Operations = cmds } -> not <| checkPreconditions (c.Model()) cmds)
        test <@ counterexample.IsNone @>

    let checkFaultyCommandModelSpecNoShrink =
        let create = StateMachine.setup (fun () -> SimpleModel()) (fun () -> 0)
        { new Machine<SimpleModel,int>() with
            member __.Setup = Gen.constant create |> Arb.fromGen
            member __.Next _ = FaultyCmd.Arb.Generator 
            override __.ShrinkOperations _ = Seq.empty }

    [<Fact>]
    let ``should not shrink if shrinking is disabled``() =
         let create = StateMachine.setup (fun () -> SimpleModel()) (fun () -> 0)
         let run = { Setup = (0,create)
                     TearDown = checkFaultyCommandModelSpecNoShrink.TearDown
                     Operations = [(FaultyInc(1) :> Operation<SimpleModel,int>,1)
                                   (FaultyInc(2) :> Operation<SimpleModel,int>,2)]
                     UsedSize = 2 }
 
         //since shrinker is disabled should not generate values through shrinking
         let shrunk = StateMachine.shrink checkFaultyCommandModelSpecNoShrink run |> List.ofSeq
         test <@ shrunk.Length = 0 @>
  

    //a counter that never goes below zero
    type Counter(?init:int) =
      let mutable n = defaultArg init 0
      member __.Inc() = 
        //if n <= 3  then n <- n + 1 else n <- n + 2
        n <- n + 1
        n
      member __.Dec() = if n <= 0 then failwithf "Precondition fail" else n <- n - 1; n
      member __.Reset() = n <- 0
      override __.ToString() = sprintf "Counter = %i, init = %A" n init

    let spec =
        let inc = 
            { new Operation<Counter,int>() with
                member __.Run m = m + 1
                member __.Check (c,m) = 
                    let res = c.Inc() 
                    m = res 
                    |@ sprintf "Inc: model = %i, actual = %i" m res
                override __.ToString() = "inc"}
        let dec = 
            { new Operation<Counter,int>() with
                member __.Run m = m - 1
                override __.Pre m = 
                    m > 0
                member __.Check (c,m) = 
                    let res = c.Dec()
                    m = res 
                    |@ sprintf "Dec: model = %i, actual = %i" m res
                override __.ToString() = "dec"}
        let create init = 
            { new Setup<Counter,int>() with
                member __.Actual() = new Counter(init)
                member __.Model() = init }
        { new Machine<Counter,int>() with
            member __.Setup = 
                Arb.fromGenShrink(
                    Gen.choose (0,100) |> Gen.map create ,
                    fun setup -> seq { if setup.Model() > 0 then yield create (setup.Model()-1) })
            member __.Next _ = Gen.elements [ inc; dec ] }

    [<Property>]
    let ``check Counter``() =
        StateMachine.toProperty spec

    // "symbolic" model

    type ActualState() =
        let mutable state = "A"
        member __.Set s = state <- s
        member __.Get = state
        override __.ToString() = state

    type ModelState = A | B | C with
        member t.Check (act:ActualState) =
            match t with
            | A -> act.Get = "A"
            | B -> act.Get = "B"
            | C -> act.Get = "C"
    
    let makeOperations failureState =
         let makeOperation failureState actualNextState startState endState =
            { new Operation<ActualState,ModelState>() with 
                override t.Run m = 
                    if not (t.Pre m) then failwithf "Run should never be called if precondition not satisfied. m = %A" m
                    endState
                override __.Pre m = m = startState
                override __.Check (act,model) = 
                    act.Set actualNextState
                    if failureState = act.Get then 
                        false
                        |@ sprintf "failurestate: model = %A, actual = %A" model act
                    else 
                        model.Check act
                        |@ sprintf "model = %A, actual = %A" model act
                override __.ToString() = sprintf "%A->%A" startState endState }

         ( makeOperation failureState "B" A B
         , makeOperation failureState "A" B A
         , makeOperation failureState "C" B C
         , makeOperation failureState "C" A C )

    let setup = 
            { new Setup<ActualState,ModelState>() with
                member __.Actual() = ActualState()
                member __.Model() =  A }

    let specSymbolic failureState =
        let (|GenConst|) x = Gen.constant x
        let (GenConst ``a->b``, GenConst ``b->a``, GenConst ``b->c``, GenConst ``a->c``) = makeOperations failureState
        
        { new Machine<ActualState,ModelState>() with
            member __.Setup = Gen.constant setup |> Arb.fromGen
            member __.Next _ = Gen.frequency [ (10,``a->b``); (1,``b->c``);  (10, ``b->a``); (1,``a->c``) ] }

    [<Property>]
    let ``check specSymbolic``() =
        StateMachine.toProperty (specSymbolic "nofail")

    [<Fact>]
    let ``shrinker should remove loops``() =
        //this to check that the shrinker can remove loops, or sequences of superfluous transitions "in the middle" of a run.
        //the standard list shrinker does not do this.
        let spec = specSymbolic "C"
        let (``a->b``, ``b->a``, ``b->c``, ``a->c``) = makeOperations "C"
        let run = { Setup = (A, setup)
                    TearDown = spec.TearDown
                    Operations = [(``a->b``,B); (``b->a``,A);(``a->c``,C)] 
                    UsedSize = 3}
        let shrunk = StateMachine.shrink spec run
        test <@ shrunk |> Seq.exists (fun e -> e.Operations = [(``a->c``,C)]) @>

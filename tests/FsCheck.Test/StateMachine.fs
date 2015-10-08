namespace FsCheck.Experimental.Test

module StateMachine =

    open Xunit
    open FsCheck
    open FsCheck.Experimental
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Swensen.Unquote

    type SimpleModel() =
        let count = ref 0
        member __.Inc() = incr count
        member __.Get = !count

    let checkSimpleModelSpec =
        let inc = StateMachine.operation "inc" ((+) 1) (fun (actual:SimpleModel,model) -> actual.Get = model)
        let create = StateMachine.setup (fun () -> SimpleModel()) (fun () -> 0)
        { new Machine<_,_>() with
            member __.Setup = Gen.constant create |> Arb.fromGen
            member __.Next _ = Gen.constant inc }

    [<Fact>]
    let ``check generated commands``() =
        let commands =
            StateMachine.generate checkSimpleModelSpec
            |> Gen.sample 10 10

        for { Setup = _,create; Operations = comms } in commands do
            typeof<SimpleModel> =! create.Actual().GetType()
            0 =! create.Model()
            for m,comm in comms do
                test <@ comm.Pre 5 @>
                6 =! comm.Run 5


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


    let inline checkPreconditions initial (cmds:seq<_*Operation<_,_>>) =
        cmds 
        |> Seq.fold (fun (model,pres) (m,cmd) -> cmd.Run model,pres && cmd.Pre model) (initial, true)
        |> snd

    [<Fact>]
    let ``generate commands should never violate precondition``() =
        test <@ StateMachine.generate checkPreconditionSpec
                |> Gen.sample 100 10
                |> Seq.forall (fun { Setup = _,c; Operations = cmds } -> checkPreconditions (c.Model()) cmds) @>

    [<Fact>]
    let ``shrink commands should never violate precondition``() =
        test <@ StateMachine.generate checkPreconditionSpec 
                |> Gen.sample 100 10
                |> Seq.map (StateMachine.shrink checkPreconditionSpec) 
                |> Seq.concat
                |> Seq.forall (fun { Setup = _,c; Operations = cmds } -> checkPreconditions (c.Model()) cmds) @>

    //a counter that never goes below zero
    type Counter(?dontcare:int) =
      let mutable n = 0
      member __.Inc() = n <- n + 1; n 
      member __.Dec() = if n <= 0 then failwithf "Precondition fail" else n <- n - 1; n
      member __.Reset() = n <- 0
      override __.ToString() = sprintf "Counter = %i, don't care = %i" n (defaultArg dontcare 0)

    let spec =
        let inc = 
            Gen.constant <|
            { new Operation<Counter,int>() with
                member __.Run m = m + 1
                member __.Check (c,m) = 
                    let res = c.Inc() 
                    m = res 
                    |@ sprintf "model = %i, actual = %i" m res
                override __.ToString() = "inc"}
        let dec = 
            Gen.constant <|
            { new Operation<Counter,int>() with
                member __.Run m = m - 1
                override __.Pre m = 
                    m > 0
                member __.Check (c,m) = 
                    let res = c.Dec()
                    m = res 
                    |@ sprintf "model = %i, actual = %i" m res
                override __.ToString() = "dec"}
        let create dontcare = 
            { new Setup<Counter,int>() with
                member __.Actual() = new Counter(dontcare)
                member __.Model() = 0 }
        { new Machine<Counter,int>() with
            member __.Setup = gen { let! dontcare = Gen.choose (0,100) in return create dontcare } |> Arb.fromGen
            member __.Next _ = Gen.oneof [ inc; dec ] }

    [<Fact>]
    let ``should check Counter``() =
        let prop = StateMachine.toProperty spec
        Check.Quick prop


namespace FsCheck.Test

module Commands =

    open Xunit
    open FsCheck
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

    type SimpleModel() =
        let count = ref 0
        member __.Inc() = incr count
        member __.Get = !count

    let checkSimpleModelSpec =
        let inc = Command.fromFun "inc" ((+) 1) (fun (actual:SimpleModel,model) -> actual.Get = model)
        let create = Command.create (fun () -> SimpleModel()) (fun () -> 0)
        { new CommandGenerator<_,_>() with
            member __.Create = Gen.constant create |> Arb.fromGen
            member __.Next _ = Gen.constant inc }

    [<Fact>]
    let ``check generated commands``() =
        let commands =
            Command.generate checkSimpleModelSpec
            |> Gen.sample 10 10
        for (create,comms,_) in commands do
            Assert.IsType<SimpleModel>(create.Actual()) |> ignore
            Assert.Equal(0, create.Model())
            for comm in comms do
                Assert.True(comm.Pre 5)
                Assert.Equal(6, comm.RunModel 5)


    //this spec is created using preconditions such that the only valid sequence is setFalse,setTrue
    //repeated 0 or more times. To simplify the test it doesn't even use an actual object under test;
    //the specification is just meant to check that preconditions are handled correctly.
    let checkPreconditionSpec =
        let setTrue = 
            Command.fromFunWithPrecondition "setTrue" not (fun _ -> true) (fun (_, _) -> true)
        let setFalse = 
            Command.fromFunWithPrecondition "setFalse" id (fun _ -> false) (fun (_, _) -> true)
        let create =
            Command.create id (fun () -> true)
        { new CommandGenerator<_,_>() with
            member __.Create = Gen.constant create |> Arb.fromGen
            member __.Next _ = Gen.elements [setTrue; setFalse] }


    let inline checkPreconditions initial (cmds:seq<Command<_,_>>) =
        cmds 
        |> Seq.fold (fun (model,pres) cmd -> cmd.RunModel model,pres && cmd.Pre model) (initial, true)
        |> snd

    [<Fact>]
    let ``generate commands should never violate precondition``() =
        Command.generate checkPreconditionSpec
        |> Gen.sample 100 10
        |> Seq.forall (fun (c,cmds,_) -> checkPreconditions (c.Model()) cmds)
        |> Assert.True
        

    [<Fact>]
    let ``shrink commands should never violate precondition``() =
        Command.generate checkPreconditionSpec 
        |> Gen.sample 100 10
        |> Seq.map (Command.shrink checkPreconditionSpec) 
        |> Seq.concat
        |> Seq.forall (fun (c,cmds,_) -> checkPreconditions (c.Model()) cmds)
        |> Assert.True
        

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
            { new Command<Counter,int>() with
                member __.RunModel m = m + 1
                member __.Check (c,m) = 
                    let res = c.Inc() 
                    m = res 
                    |@ sprintf "model = %i, actual = %i" m res
                override __.ToString() = "inc"}
        let dec = 
            Gen.constant <|
            { new Command<Counter,int>() with
                member __.RunModel m = m - 1
                override __.Pre m = 
                    m > 0
                member __.Check (c,m) = 
                    let res = c.Dec()
                    m = res 
                    |@ sprintf "model = %i, actual = %i" m res
                override __.ToString() = "dec"}
        let create dontcare = 
            { new Create<Counter,int>() with
                member __.Actual() = new Counter(dontcare)
                member __.Model() = 0 }
        { new CommandGenerator<Counter,int>() with
            member __.Create = gen { let! dontcare = Gen.choose (0,100) in return create dontcare } |> Arb.fromGen
            member __.Next _ = Gen.oneof [ inc; dec ] }

    [<Fact>]
    let ``should check Counter``() =
        let prop = Command.toProperty spec
        Check.Quick prop


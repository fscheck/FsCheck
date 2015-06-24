namespace FsCheck.Test

module Commands =

    open Xunit
    open FsCheck

    type SimpleModel() =
        let count = ref 0
        member __.Inc() = incr count
        member __.Get = !count

    let checkSimpleModelSpec =
        let inc = Command.fromFun ((+) 1) (fun (actual:SimpleModel,model) -> actual.Get = model)
        let create = Command.create (fun () -> SimpleModel()) (fun () -> 0)
        { new CommandGenerator<_,_>() with
            member __.Create = Gen.constant create
            member __.Next model = Gen.constant inc }

    [<Fact>]
    let ``check generated commands``() =
        let commands =
            Command.generator checkSimpleModelSpec
            |> Gen.sample 10 10
        for (create,comms,_) in commands do
            Assert.IsType<SimpleModel>(create.Actual()) |> ignore
            Assert.Equal(0, create.Model())
            for comm in comms do
                Assert.True(comm.Pre 5)
                Assert.Equal(6, comm.RunModel 5)

    type CheckPrecondition() =
        let mutable b = true
        member __.SetTrue() = if b then invalidOp "Precondition violated" else b <- true; true
        member __.SetFalse() = if not b then invalidOp "Precondition violated" else b <- false; false

    let checkPreconditionSpec =
        let setTrue = 
            Command.fromFunWithPrecondition not (fun _ -> true) (fun (actual:CheckPrecondition, model) -> actual.SetTrue() = model)
        let setFalse = 
            Command.fromFunWithPrecondition id (fun _ -> false) (fun (actual:CheckPrecondition, model) -> actual.SetFalse() = model)
        let create =
            Command.create (fun () -> CheckPrecondition()) (fun () -> true)
        { new CommandGenerator<_,_>() with
            member __.Create = Gen.constant create
            member __.Next model = Gen.elements [setTrue; setFalse]}


    [<Fact>]
    let ``generated commands should never violate precondition``() =
        checkPreconditionSpec
        |> Command.toProperty
        |> Check.QuickThrowOnFailure


    //a counter that never goes below zero
    type Counter() =
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
            member __.Create = gen { let! dontcare = Gen.choose (0,100) in return create dontcare }
            member __.Next _ = Gen.frequency [ 1, inc
                                               1, dec ] }

    [<Fact>]
    let ``should check Counter``() =
        let prop = Command.toProperty spec
        Check.Quick prop


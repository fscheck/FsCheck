namespace FsCheck.Test

module Commands =

    open Xunit
    open FsCheck

    //a counter that never goes below zero
    type Counter() =
      let mutable n = 0
      member __.Inc() = n <- n + 1 //if n > 5 then n <- n + 2 else n <- n + 1
      member __.Dec() = if n = 0 then failwithf "Precondition fail" else n <- n - 1
      member __.Get = n
      member __.Reset() = n <- 0
      override __.ToString() = n.ToString()

    let spec =
        let inc = 
            Gen.constant <|
            { new Command<Counter,int>() with
                member __.RunActual c = c.Inc(); c
                member __.RunModel m = m + 1
                member __.Post (c,m) = m = c.Get |@ sprintf "m = %i, c = %i" m c.Get
                override __.ToString() = "inc"}
        let dec = 
            Gen.constant <|
            { new Command<Counter,int>() with
                member __.RunActual c = c.Dec(); c
                member __.RunModel m = m - 1
                member __.Pre m = m > 0
                member __.Post (c,m) = m = c.Get |@ sprintf "m = %i, c = %i" m c.Get
                override __.ToString() = "dec"}
        { new ICommandGenerator<Counter,int> with
            member __.InitialActual = Counter()
            member __.InitialModel = 0
            member __.Next _ = Gen.frequency [ 2, inc //otherwise, args exhausted
                                               1, dec ] }

    [<Fact>]
    let ``should check Counter``() =
        let prop = Command.toProperty spec
        Check.QuickThrowOnFailure prop

    [<Fact>]
    let ``should check Counter no shrink``() =
        let prop = Command.toPropertyWith spec (Command.generateCommands spec) (fun _ -> Seq.empty)
        Check.QuickThrowOnFailure prop

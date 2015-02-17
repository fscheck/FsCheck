namespace FsCheck.Test

module Commands =

    open FsCheck
    open FsCheck.Xunit

    //a counter that never goes below zero
    type Counter() =
      let mutable n = 0
      member x.Inc() = n <- n + 1
      member x.Dec() = if n = 0 then failwithf "Precondition fail" else n <- n - 1
      member x.Get = n
      member x.Reset() = n <- 0
      override x.ToString() = n.ToString()

    let spec =
        let inc = 
            Gen.constant <|
            { new Command<Counter,int>() with
                member x.RunActual c = c.Inc(); c
                member x.RunModel m = m + 1
                member x.Post (c,m) = m = c.Get |@ sprintf "m = %i, c = %i" m c.Get
                override x.ToString() = "inc"}
        let dec = 
            Gen.constant <|
            { new Command<Counter,int>() with
                member x.RunActual c = c.Dec(); c
                member x.RunModel m = m - 1
                member x.Pre m = m > 0
                member x.Post (c,m) = m = c.Get |@ sprintf "m = %i, c = %i" m c.Get
                override x.ToString() = "dec"}
        { new ICommandGenerator<Counter,int> with
            member __.InitialActual = Counter()
            member __.InitialModel = 0
            member __.Next _ = Gen.frequency [ 2, inc //otherwise, args exhausted
                                               1, dec ] }

    [<Property>]
    let ``should check Counter``() =
        Command.asProperty spec


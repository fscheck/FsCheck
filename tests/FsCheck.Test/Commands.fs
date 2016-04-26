namespace FsCheck.Test

module Commands =

    open Xunit
    open FsCheck
    open Swensen.Unquote

    //a counter that never goes below zero
    type Counter() =
      let mutable n = 0
      member __.Inc() = n <- n + 1 
        //if n > 5 then n <- n + 2 else n <- n + 1
      member __.Dec() = if n <= 0 then failwithf "Precondition fail" else n <- n - 1
      member __.Get = n
      member __.Reset() = n <- 0
      override __.ToString() = n.ToString()

    
    let inc = 
        { new Command<Counter,int>() with
            member __.RunActual c = c.Inc(); c
            member __.RunModel m = m + 1
            member __.Post (c,m) = m = c.Get |@ sprintf "m = %i, c = %i" m c.Get
            override __.ToString() = "inc"}
    let dec = 
        { new Command<Counter,int>() with
            member __.RunActual c = c.Dec(); c
            member __.RunModel m = m - 1
            member __.Pre m = m > 0
            member __.Post (c,m) = m = c.Get |@ sprintf "m = %i, c = %i" m c.Get
            override __.ToString() = "dec"}
    let spec =
        let incOrDec = Gen.elements [inc; dec]
        let inc = Gen.constant inc
        { new ICommandGenerator<Counter,int> with
            member __.InitialActual = Counter()
            member __.InitialModel = 0
            member __.Next m = if m > 0 then incOrDec else inc
         }


    [<Fact>]
    let ``should check Counter``() =
        let prop = Command.toProperty spec
        Check.QuickThrowOnFailure prop

    [<Fact>]
    let ``should check Counter no shrink``() =
        let prop = Command.toPropertyWith spec (Command.generate spec) (fun _ -> Seq.empty)
        Check.QuickThrowOnFailure prop

    [<Fact>]
    let ``should check preconditions during shrinking``() =
        //bit of a silly test...this sequence already fails the precondition, but [inc] does
        //pass the preconditions. So we check that that is the only item that remains.
        let sequence = [dec;inc]
        let shrunk = Command.shrink spec sequence |> Seq.map Seq.toList |> Seq.toList
        test <@ shrunk = [List.tail sequence] @>
        


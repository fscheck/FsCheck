module StatefulTesting

open FsCheck
open Commands

type Counter() =
  let mutable n = 0
  member x.Inc() = n <- n + 1
  member x.Dec() = if n > 2 then n <- n - 2 else n <- n - 1
  member x.Get = n
  member x.Reset() = n <- 0
  override x.ToString() = n.ToString()

let spec =
    let inc = 
        { new ICommand<Counter,int>() with
            member x.RunActual c = c.Inc(); c
            member x.RunModel m = m + 1
            member x.Post (c,m) = m = c.Get 
            override x.ToString() = "inc"}
    let dec = 
        { new ICommand<Counter,int>() with
            member x.RunActual c = c.Dec(); c
            member x.RunModel m = m - 1
            member x.Post (c,m) = m = c.Get 
            override x.ToString() = "dec"}
    { new ISpecification<Counter,int> with
        member x.Initial() = (new Counter(),0)
        member x.GenCommand _ = Gen.elements [inc;dec] }

Check.Quick (asProperty spec)


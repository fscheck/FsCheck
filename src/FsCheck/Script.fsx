open System
open System.Reflection
open Microsoft.FSharp.Reflection

#r @"bin\Debug\netstandard2.0\FsCheck.dll"
open FsCheck
open FsCheck.FSharp
open FsCheck.Experimental
open System.Collections.Generic

//type Built<'T> = 
//    | Success of (unit->'T)
//    | Failure of (unit->'T)
//    | Reject of (unit->'T)

//type BuilderOverload() =
//    member _.Return (x:unit) = 
//        printfn "Return unit"
//        Success
//    member _.Return (x:bool) = 
//        printfn "Return bool"
//        if x then Success else Failure
//    member _.Combine(B l, B r) =
//        if l
//    member _.Bind((B m:Built), f) :Built =
//        printfn "Bind"
//        f m
//    member _.Delay(f: unit -> 'a) =
//        printfn "Delay"
//        B f
//    //member _.Run(B f:Built<unit->'a>) =
//    //    f()

//let b = new BuilderOverload()

//let res = b {
//    return ()
//    return bool
//}




let l = Gen.listOf (Gen.constant 1)
l 
|> Gen.sampleWithSize 10 1000
|> Seq.map Seq.length 
|> Seq.groupBy id 
|> Seq.map (fun (l,gr) -> (l, Seq.length gr))
|> Seq.sortBy fst
|> Seq.toArray


type Pid = int
type Name = string
type ProcessRegistry() =
    let pids = new List<Pid>()
    let registry = new Dictionary<Name,Pid>()
    let mutable pidCounter = 0
    member __.Spawn() : Pid = 
        let pid = pidCounter
        pidCounter <- pidCounter + 1
        pids.Add pid
        pid
    member __.Kill(pid:Pid) : unit = 
        pids.Remove(pid) |> ignore
        let unregs =
            registry 
            |> Seq.where (fun kvp -> kvp.Value = pid) 
            |> Seq.map (fun kvp -> kvp.Key)
            |> Seq.toArray
        unregs |> Seq.iter (registry.Remove >> ignore)
    member __.Reg(name:Name, pid:Pid) =
        if pids.Contains pid then
            registry.Add(name,pid)
    member __.Where(name:Name) : Pid =
        registry.[name]
    member __.Unreg(name:Name) =
        registry.Remove(name)

type ProcRegModel = 
    { Pids:list<OperationResult<Pid>>
      Regs: list<Name*OperationResult<Pid>>
      Killed: list<OperationResult<Pid>>}

let procRegSpec =
    let setup = 
        { new Setup<ProcessRegistry, ProcRegModel>() with 
            override __.Actual() = ProcessRegistry() 
            override __.Model() = { Pids = []; Regs = []; Killed = []}
        }
    let spawn pid = 
        { new Operation<ProcessRegistry, ProcRegModel>() with
            override __.Run m = { m with Pids = pid::m.Pids}
            override op.Check (actual,model) =
                let r = actual.Spawn()
                pid.V(op) <- r
                true.ToProperty()
            override __.ToString() = sprintf "%O = spawn()" pid
        }
    let kill (pid:OperationResult<Pid>) = 
        { new Operation<ProcessRegistry, ProcRegModel>() with
            override __.Run m = { m with Killed = pid::m.Killed; Regs = m.Regs |> List.where (fun (_,p) -> pid <> p)}
            override op.Check (actual,model) =
                actual.Kill(pid.V(op))
                true.ToProperty()
            override __.ToString() = sprintf "kill %O" pid
        }

    let reg name pid =
            
        let nameExists name { Regs = regs } =
            List.exists (fst >> (=) name) regs
        { new Operation<ProcessRegistry, ProcRegModel>() with
            override __.Run m = 
                if nameExists name m || List.exists ((=) pid) m.Killed then
                    m
                else
                    { m with Regs = (name,pid)::m.Regs }
            override op.Check (actual,model) =
                try
                    actual.Reg (name, pid.V(op))
                    (nameExists name model || List.exists ((=) pid) model.Killed)
                        |> Prop.label "register succeeded but shouldn't work"
                with e ->
                    nameExists name model |> Prop.label (sprintf "register failed but should work: %O" e)
            override __.ToString() = sprintf "reg %s %O" name pid
        }
    let unreg name =
        let unregisterOk name { Regs = regs } =
            List.exists (fun (n,_) -> n = name) regs

        { new Operation<ProcessRegistry, ProcRegModel>() with
            override __.Run m = 
                { m with Regs = m.Regs |> List.where (fun (e,_) -> e <> name)  }
            override __.Check (actual,model) =
                let r = actual.Unreg name
                r = unregisterOk name model |> Prop.label (sprintf "unregister result was %b but should have been %b" r (not r))
                |> Prop.ofTestable
            override __.ToString() = sprintf "unreg %s" name
        }
    let name = Gen.elements ["a"; "b"; "c"; "d"]
    //these are always used in next no matter what the state of teh model - they are here
    //for efficiency purposes (i.e. avoid re-creating them each time we call Next.
    let spawnOrUnreg = [ Gen.fresh (fun () -> spawn (OperationResult())) (*; Gen.map unreg name*) ]
    { new Machine<ProcessRegistry, ProcRegModel>() with
        override __.Setup = setup |> Gen.constant |> Arb.fromGen
        override __.Next { Pids = pids } =
            let killOrReg = 
                match pids with 
                | [] -> []
                | _ -> let pids = Gen.elements pids 
                       [ Gen.map kill pids ; Gen.map2 reg name pids ]
            Gen.oneof (killOrReg @ spawnOrUnreg)
    }
    
let p = StateMachine.toProperty procRegSpec
Check.Quick p

type Counter(initial:int) =
    let mutable c = initial
    member __.Inc() = c <- c + 1
    member __.Dec() = c <- c- 1
    member __.Add(i:int) = c <- c + i
    member __.Get = c
    interface IDisposable with
        member x.Dispose(): unit = 
            ()
        

let spec = ObjectMachine<Counter>(ArbMap.defaults)
let generator = StateMachine.generate spec

let sample = generator |> Gen.sampleWithSize 10 1 |> Seq.head

StateMachine.toProperty spec |> Check.Verbose
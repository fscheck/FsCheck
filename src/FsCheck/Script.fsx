open System
open System.Reflection
open Microsoft.FSharp.Reflection

#r "bin\Debug\FsCheck.dll"
open FsCheck
open FsCheck.Experimental
open System.Collections.Generic

type Pid = int
type Name = string
type ProcessRegistry() =
    let pids = new System.Collections.Generic.List<Pid>()
    let registry = new System.Collections.Generic.Dictionary<Name,Pid>()
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
            override __.Actual() = new ProcessRegistry() 
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
                        |@ "register succeeded but shouldn't work"
                with e ->
                    nameExists name model |@ sprintf "register failed but should work: %O" e
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
                r = unregisterOk name model |@ sprintf "unregister result was %b but should have been %b" r (not r)
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
//Check.One( { Config.Quick with Replay=Some <| Random.StdGen (295795725,296144228)}, )
//Check.One( { Config.Quick with Replay=Some <| Random.StdGen (1145655947,296144285); EveryShrink = fun args -> sprintf "%A" args}, p )

//let rec subsequences l = //all subsequences is like binary counting...
//    match l with
//    | [] -> []
//    | [x] -> [[]]
//    | x::xs -> 
//        let r = subsequences xs
//        printf "."
//        (List.map (fun ys -> x :: ys) r) @ xs :: r


type Counter(initial:int) =
    let mutable c = initial
    member __.Inc() = c <- c + 1
    member __.Dec() = c <- c- 1
    member __.Add(i:int) = c <- c + i
    member __.Get = c
    interface IDisposable with
        member x.Dispose(): unit = 
            ()
        

let spec = ObjectMachine<Counter>()
let generator = StateMachine.generate spec

let sample = generator |> Gen.sampleWithSize 10 1 |> Seq.head

StateMachine.toProperty spec |> Check.Verbose

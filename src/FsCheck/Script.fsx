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
    member __.Spawn() : Pid = 
        let pid = pids.Count
        pids.Add pid
        pid
    member __.Kill(pid:Pid) : unit = 
        pids.Remove(pid) |> ignore
    member __.Reg(name:Name, pid:Pid) =
        if not (pids.Contains pid) then failwithf "Pid %i does not exist" pid
        registry.Add(name,pid);
    member __.Where(name:Name) : Pid =
        registry.[name]
    member __.Unreg(name:Name) =
        registry.Remove(name)

type ProcRegModel = { Pids:list<OperationResult<Pid>>; Regs: list<Name*OperationResult<Pid>>; Killed: list<OperationResult<Pid>>}

let procRegSpec =
    let setup = 
        { new Setup<ProcessRegistry, ProcRegModel>() with 
            override __.Actual() = new ProcessRegistry() 
            override __.Model() = { Pids = []; Regs = []; Killed = []}
        }
    let spawn = 
        { new Operation<ProcessRegistry, ProcRegModel>() with
            override __.Run m = { m with Pids = OperationResult<Pid>()::m.Pids}
            override op.Check (actual,model) =
                let r = actual.Spawn()
                model.Pids.Head.V(op) <- r
                true.ToProperty()
            override __.ToString() = "spawn"
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
        let registerOk name { Regs = regs } =
            List.forall (fun (n,_) -> n <> name) regs
        let nameExists name { Regs = regs } =
            List.exists (fst >> (=) name) regs
        { new Operation<ProcessRegistry, ProcRegModel>() with
            override __.Run m = 
                if not (registerOk name m) || List.exists ((=) pid) m.Killed then
                    m
                else
                    { m with Regs = (name,pid)::m.Regs }
            override op.Check (actual,model) =
                try
                    actual.Reg (name, pid.V(op))
                    nameExists name model |@ "register succeeded but shouldn't work"
                with e ->
                    not (nameExists name model) |@ "register failed but should work"
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
    let spawnOrUnreg = [ Gen.constant spawn (*; Gen.map unreg name*) ]
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
Check.One( { Config.Quick with Replay=Some <| Random.StdGen (1145655947,296144285)}, p )

let rec subsequences l = //all subsequences is like binary counting...
    match l with
    | [] -> []
    | [x] -> [[]]
    | x::xs -> 
        let r = subsequences xs
        printf "."
        (List.map (fun ys -> x :: ys) r) @ xs :: r

//how to generate float between 0 and 5
//let f = 
//    Arb.generate<NormalFloat>
//    |> Gen.map float
//    |> Gen.resize 5
//    |> Gen.map abs
//    |> Gen.suchThat (fun f -> f < 5.)
//    |> Gen.sample 100 10

type Arbs =
  static member SafeString () =
    Arb.Default.String () |> Arb.filter (fun str -> str <> null && not (str = ""))

  static member NonEmptyStringMaps () =
    Arb.Default.Map () |> Arb.filter (fun m -> m |> Map.toList |> List.forall (fun (k,v) -> k <> "" && k <> null))

  static member DateTime () =
    Arb.Default.DateTime ()
    |> Arb.mapFilter (fun dt -> dt.ToUniversalTime()) (fun dt -> dt.Kind = System.DateTimeKind.Utc)

let fsCheckConfig = { Config.Default with Arbitrary = [ typeof<Arbs> ] }

Check.One(fsCheckConfig,(fun (s:string,m:Map<string,int>) -> not <| String.IsNullOrEmpty s && m |> Map.forall (fun s v -> not <| String.IsNullOrEmpty s)))

Check.One(fsCheckConfig,(fun (s:DateTime) -> s.Kind = DateTimeKind.Utc))

//Gen.sample 10 1 <| Arbs.DateTime().Generator

#time

Check.Verbose (fun x -> x < 10)

type MyUnion = Card of int | Suit of string | Foo of float * list<int>

Arb.generate<MyUnion> |> Gen.sample 100 10



//type UnionCase =
//    | Single
//    | One of int
//    | Two of string * int
//    | Three of string * char * int
//    | Rec of int * UnionCase
//
//let testProp (uc : UnionCase) (uc2:UnionCase) (uc3:UnionCase) =
//  true
//
//let testPerf () =
//  let config =
//    {
//      Config.Quick with
//        MaxTest = 1000
//    }
//  Check.One (config, testProp)
//
//testPerf()
//
//let run() =
//    let res = Arb.from<UnionCase>.Generator |> Gen.sample 100 100000
//    sprintf "%i" res.Length
//
//run()

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

let sample = generator |> Gen.sample 10 1 |> Seq.head

StateMachine.toProperty spec |> Check.Verbose

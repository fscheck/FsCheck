open System
open System.Reflection
open Microsoft.FSharp.Reflection

#r "bin\Debug\FsCheck.dll"
open FsCheck
open FsCheck.Experimental
open System.Collections.Generic

false < true
uint64 Int64.MaxValue + uint64 Int64.MaxValue + 1UL
13848750606405358206UL  < 9250757139101164796UL

9250757139101164796UL - uint64 Int64.MaxValue

//Int64.MinValue + Int64.MaxValue + 1L |> uint64

/// Preconditions: 
/// 1. current <> target
/// 2. Must be signed type
let inline moveCloser current target (overflow:byref<bool>) (diff:byref<_>) =
    let one = LanguagePrimitives.GenericOne
    let two = one + one
    diff <- current - target
    // this is why the precondition is necessary: if current = target, the else branch detects
    // a false overflow for MinValue and MinValue
    overflow <- if target > LanguagePrimitives.GenericZero then diff > current else diff < current
    //if current > target then diff < current else current < diff
    if overflow then
        if current > target then
            current / two - one
        else
            current / two + one
    else
        current - diff / two
        //if current > target then current - diff / two
        //else current + diff / two

let inline shrink current target =
    seq {  if current <> target then 
               yield target
               let mutable overflow = false
               let mutable diff = LanguagePrimitives.GenericZero
               let mutable c = moveCloser current target &overflow &diff
               while overflow || abs diff > LanguagePrimitives.GenericOne do
                    yield c
                    c <- moveCloser c target &overflow &diff //c - (c - target) / 2L
        }
shrink 110y 100y |> Seq.toArray
shrink 100y 110y |> Seq.toArray
shrink -110y 100y |> Seq.toArray
shrink 100y -110y |> Seq.toArray
shrink 100y 0y |> Seq.toArray
shrink -100y 0y |> Seq.toArray

//shrink 110uy 100uy |> Seq.toArray
//shrink 100uy 110uy |> Seq.toArray


shrink SByte.MaxValue SByte.MinValue |> Seq.toArray
shrink SByte.MinValue SByte.MaxValue |> Seq.toArray
shrink SByte.MinValue SByte.MinValue |> Seq.toArray
shrink SByte.MaxValue SByte.MaxValue |> Seq.toArray

//shrink Byte.MaxValue Byte.MinValue |> Seq.toArray
//shrink Byte.MinValue Byte.MaxValue |> Seq.toArray
//shrink Byte.MinValue Byte.MinValue |> Seq.toArray
//shrink Byte.MaxValue Byte.MaxValue |> Seq.toArray


SByte.MaxValue - SByte.MinValue
SByte.MinValue - SByte.MaxValue

let inline overflows current target =
    printfn "current = %A target = %A" current target
    let diff = current - target
    printfn "diff = %A" diff
    // this is why the precondition is necessary: if current = target, the else branch detects
    // a false overflow for MinValue and MinValue
    //if current > target then diff < current else current < diff
    if target > LanguagePrimitives.GenericZero then diff > current else diff < current

overflows SByte.MaxValue SByte.MinValue = true
overflows SByte.MinValue SByte.MaxValue = true
overflows 110y 100y = false
overflows 100y 110y = false
overflows -63y 127y = true

overflows Byte.MaxValue Byte.MinValue
overflows Byte.MinValue Byte.MaxValue
overflows 110uy 100uy
overflows 100uy 110uy


Int64.MinValue < (Int64.MaxValue - Int64.MinValue)
a - b = r => a >= r 
a = b  + r

let inline printBits (v:int64) = printfn "%64s" <| Convert.ToString(v,2)

let target = 1234243L
let value = 99999999L
let differentBits = target ^^^ value

let setHighestBitInMaskToZero mask (originalValue:int64) =
    let mutable mask = mask;

    mask <- mask ||| (mask >>> 1)
    mask <- mask ||| (mask >>> 2)
    mask <- mask ||| (mask >>> 4)
    mask <- mask ||| (mask >>> 8)
    mask <- mask ||| (mask >>> 16)
    mask <- mask ||| (mask >>> 32)
    
    mask <- mask >>> 1;

    originalValue &&& mask

let v2 = setHighestBitInMaskToZero differentBits value

printBits target
printBits value
printBits differentBits
printBits v2

t  v
00 00 -> []
01 00 -> [01]
10 00 -> [10]
11 00 -> [01;10]

0101
1011

0101  clear 1000 and set 0100
0011  clear 0100 and srt 01=010
0001


Arb.generate<list<int>> |> Gen.sampleWithSize 50 10

Prop.forAll (Arb.generate<int>) (fun (a:int) -> printfn "%i" a)
|> Check.Quick

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

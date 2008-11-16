#light

open System
open System.Collections.Generic
open System.Reflection
open Random
open Microsoft.FSharp.Control
//open Microsoft.FSharp.Reflection
 
type IGen = 
    abstract member AsGenObject : Gen<obj>
and Gen<'a> = 
    Gen of (int -> StdGen -> 'a)   
        member x.Map<'a,'b> (f: 'a -> 'b) : Gen<'b> = match x with (Gen g) -> Gen (fun n r -> f <| g n r)
    interface IGen with
        member x.AsGenObject = x.Map box
        //match x with (Gen g) -> Gen (fun n r -> g n r |> box)

let sized fgen = Gen (fun n r -> let (Gen m) = fgen n in m n r)

let resize n (Gen m) = Gen (fun _ r -> m n r)

let rand = Gen (fun n r -> r)

//generates a value out of the generator with maximum size n
let generate n rnd (Gen m) = 
    let size,rnd' = range (0,n) rnd
    m size rnd'

type GenBuilder () =
    member b.Return(a) : Gen<_> = 
        Gen (fun n r -> a)
    member b.Bind((Gen m) : Gen<_>, k : _ -> Gen<_>) : Gen<_> = 
        Gen (fun n r0 -> let r1,r2 = split r0
                         let (Gen m') = k (m n r1) 
                         m' n r2)                                      
    //member b.Let(p, rest) : Gen<_> = rest p
    //not so sure about this one...should delay executing until just before it is executed,
    //for side-effects. Examples are usually like = fun () -> runGen (f ())
    member b.Delay(f : unit -> Gen<_>) : Gen<_> = Gen (fun n r -> match f() with (Gen g) -> g n r )

let gen = GenBuilder()

//haskell types are more general; this always results in an int 
let choose (l, h) = rand.Map (range (l,h) >> fst) 

let elements xs = (choose (0, (List.length xs)-1) ).Map(List.nth xs)

let oneof gens = gen.Bind(elements gens, fun x -> x)

let frequency xs = 
    let tot = List.sum_by (fun x -> x) (List.map fst xs)
    let rec pick n ys = match ys with
                        | (k,x)::xs -> if n<=k then x else pick (n-k) xs
                        | _ -> raise (ArgumentException("Bug in frequency function"))
    in gen.Bind(choose (1,tot), fun n -> pick n xs)  

let liftGen f = fun a -> gen {  let! a' = a
                                return f a' }

let liftGen2 f = fun a b -> gen {   let! a' = a
                                    let! b' = b
                                    return f a' b' }
                                    
let two g = liftGen2 (fun a b -> (a,b)) g g

let liftGen3 f = fun a b c -> gen { let! a' = a
                                    let! b' = b
                                    let! c' = c
                                    return f a' b' c' }
let three g = liftGen3 (fun a b c -> (a,b,c)) g g g

let liftGen4 f = fun a b c d -> gen {   let! a' = a
                                        let! b' = b
                                        let! c' = c
                                        let! d' = d
                                        return f a' b' c' d' }
let four g = liftGen4 (fun a b c d -> (a,b,c,d)) g g g g

let fraction (a:int) (b:int) (c:int) = 
    double a + ( double b / abs (double c)) + 1.0 

let rec sequence l = match l with
                            | [] -> gen { return [] }
                            | c::cs -> gen {let! x = c
                                            let! xs = sequence cs
                                            return  x::xs } 

let vector arbitrary n = sequence [ for i in 1..n -> arbitrary ]

let promote f = Gen (fun n r -> fun a -> let (Gen m) = f a in m n r)

let variant = fun v (Gen m) ->
    let rec rands r0 = seq { let r1,r2 = split r0 in yield! Seq.cons r1 (rands r2) } 
    Gen (fun n r -> m n (Seq.nth (v+1) (rands r)))

type Gen =
    static member Unit = gen { return () }
    static member Bool = elements [true; false]
    static member Int = sized <| fun n -> choose (-n,n)
    static member Float = liftGen3 fraction Gen.Int Gen.Int Gen.Int
    static member Char = (choose (int Char.MinValue, 127)).Map char 
    static member String = (Gen.List(Gen.Char)).Map (fun chars -> new String(List.to_array chars))
    static member Object = 
        oneof [ (Gen.Unit :> IGen).AsGenObject;
                (Gen.Bool :> IGen).AsGenObject;
                (Gen.Int :> IGen).AsGenObject; 
                (Gen.Float :> IGen).AsGenObject;
                (Gen.Char :> IGen).AsGenObject ]
    static member Tuple(a,b) = liftGen2 (fun x y -> (x,y)) a b
    static member Tuple(a,b,c) = liftGen3 (fun x y z -> (x,y,z)) a b c 
    static member Tuple(a,b,c,d) = liftGen4 (fun x y z w -> (x,y,z,w)) a b c d
    static member Option(a) = 
        let arbMaybe size = 
            match size with
                | 0 -> gen { return None }
                | n -> (a |> resize (n-1)).Map(Some) 
        in sized arbMaybe
    static member List<'a>(a : Gen<'a>) : Gen<list<'a>> = sized (fun n -> gen.Bind(choose(0,n), vector a))
    static member Arrow(coa,genb) = promote (fun a -> coa a genb)  


type Co =
    static member Unit a = variant 0
    static member Bool b = if b then variant 0 else variant 1
    static member Int n = variant (if n >= 0 then 2*n else 2*(-n) + 1)
    static member Char c = Co.Int (int c)
    static member String s = Co.List (Co.Char) s                        
    static member Tuple (coa,cob) (a,b) = coa a >> cob b
    static member Tuple (coa,cob,coc) (a,b,c) = coa a >> cob b >> coc c
    static member Tuple (coa,cob,coc,cod) (a,b,c,d) = 
          coa a >> cob b >> coc c >> cod d      
    static member Float (fl:float) = //convert float 10.345 to 10345 * 10^-3
        let d1 = sprintf "%g" fl
        let spl = d1.Split([|'.'|])
        let m = if (spl.Length > 1) then spl.[1].Length else 0
        let decodeFloat = (fl * float m |> int, m )
        Co.Tuple(Co.Int, Co.Int) <| decodeFloat
    static member Option coa a =
          match a with 
            | None -> variant 0
            | Some y -> variant 1 >> coa y                                
    static member List coa l = match l with
                                | [] -> variant 0
                                | x::xs -> coa x << variant 1 << Co.List coa xs
    static member Arrow (gena,cob) f (gn:Gen<_>) = 
        gen { let! x = gena
              return! cob (f x) gn }



type Result = { ok : option<Lazy<bool>>
                stamp : list<string>
                arguments : list<string> }

let nothing = { ok = None; stamp = []; arguments = [] }

type Property = Prop of Gen<Result>

let result res = gen { return res } |> Prop
                       
let evaluate (Prop gen) = gen

let forAll gn body = 
    let argument a res = { res with arguments = any_to_string a :: res.arguments } in
    Prop <|  gen { let! a = gn
                   let! res = (evaluate (body a))
                   return (argument a res) }

let emptyProperty = result nothing

let implies b a = if b then a else emptyProperty
let (==>) b a = implies b a

let label str a = 
    let add res = { res with stamp = str :: res.stamp } in
    Prop ((evaluate a).Map add)

let classify b name a = if b then label name a else a

let trivial b = classify b "trivial"

let collect v = label <| any_to_string v

let prop b = gen { return {nothing with ok = Some b}} |> Prop
let propl b = gen { return {nothing with ok = Some (lazy b)}} |> Prop

type TestData = { NumberOfTests: int; Stamps: seq<int * list<string>>}
type TestResult = 
    | True of TestData
    | False of TestData * list<string> //the arguments that produced the failed test
    | Exhausted of TestData
type IRunner =
    abstract member OnArguments: int * list<string> * (int -> list<string> -> string) -> unit
    abstract member OnFinished: string * TestResult -> unit

type Config = 
    { maxTest : int
      maxFail : int
      name    : string
      size    : float -> float  //determines size passed to the generator as funtion of the previous size. Rounded up.
                            //float is used to allow for smaller increases than 1.
                            //note: in QuickCheck, this is a function of the test number!
      every   : int -> string list -> string  //determines what to print if new arguments args are generated in test n
      runner  : IRunner } //the test runner  

type TestStep = 
    | Generated of list<string> //test number and generated arguments (test not yet executed)
    | Passed of list<string> //passed, test number and stamps for this test
    | Falsified of list<string>  //falsified the property with given arguments. test number and args passed again for convenience)
    | Failed //of int(*number of test*) * int (*nb of failed tests*)  //generated arguments did not pass precondition, number of already failed tests is given

let (|Lazy|) (inp:Lazy<'a>) = inp.Force()             

let rec test initSize resize rnd0 gen =
    seq { let rnd1,rnd2 = split rnd0
          let newSize = resize initSize
          let result = generate (newSize |> round |> int) rnd2 gen
          yield Generated result.arguments
          match result.ok with
            | None -> 
                yield Failed  
                yield! test newSize resize rnd1 gen
            | Some (Lazy true) -> 
                yield Passed result.stamp
                yield! test newSize resize rnd1 gen
            | Some (Lazy false) -> 
                yield Falsified result.arguments
                yield! test newSize resize rnd1 gen
    }

let testsDone config outcome ntest stamps =    
    let entry (n,xs) = (100 * n / ntest),xs
    let table = stamps 
                |> Seq.filter (fun l -> l <> []) 
                |> Seq.sort_by (fun x -> x) 
                |> Seq.group_by (fun x -> x) 
                |> Seq.map (fun (l, ls) -> (Seq.length ls, l))
                |> Seq.sort_by (fun (l, ls) -> l)
                |> Seq.map entry
                //|> Seq.to_list
                //|> display
    let testResult =
        match outcome with
            | Passed _ -> True { NumberOfTests = ntest; Stamps = table }
            | Falsified args -> False ({ NumberOfTests = ntest; Stamps = table }, args)
            | Failed _ -> Exhausted { NumberOfTests = ntest; Stamps = table }
            | _ -> failwith "Test ended prematurely"
    config.runner.OnFinished(config.name,testResult)
    //Console.Write(message outcome + " " + any_to_string ntest + " tests" + table:string)

let runner config property = 
    let testNb = ref 0
    let failedNb = ref 0
    let lastStep = ref Failed
    test 0.0 (config.size) (newSeed()) (evaluate property) |>
    Seq.take_while (fun step ->
        lastStep := step
        match step with
            | Generated args -> config.runner.OnArguments(!testNb, args, config.every); true//Console.Write(config.every !testNb args); true
            | Passed _ -> testNb := !testNb + 1; !testNb <> config.maxTest //stop if we have enough tests
            | Falsified _ -> testNb := !testNb + 1; false //falsified, always stop
            | Failed -> failedNb := !failedNb + 1; !failedNb <> config.maxFail) |> //failed, stop if we have too much failed tests
    Seq.fold (fun acc elem ->
        match elem with
            | Passed stamp -> (stamp :: acc)
            | _ -> acc
    ) [] |>   
    testsDone config !lastStep !testNb

let consoleRunner =
    let display l = match l with
                        | []  -> ".\n"
                        | [x] -> " (" + x + ").\n"
                        | xs  -> ".\n" + List.fold_left (fun acc x -> x + ".\n"+ acc) "" xs
    let rec intersperse sep l = match l with
                                | [] -> []
                                | [x] -> [x]
                                | x::xs -> x :: sep :: intersperse sep xs  
    let entry (p,xs) = any_to_string p + "% " + (intersperse ", " xs |> Seq.to_array |> String.Concat)
    let stamps_to_string s = s |> Seq.map entry |> Seq.to_list |> display
    { new IRunner with
        member x.OnArguments (ntest,args, every) =
            printf "%s" (every ntest args)
        member x.OnFinished(name,testResult) = 
            let name = (name+"-")
            match testResult with
                | True data -> printf "%sOk, passed %i tests%s" name data.NumberOfTests (data.Stamps |> stamps_to_string )
                | False (data, args) -> printf "%sFalsifiable, after %i tests: %A\n" name data.NumberOfTests args 
                | Exhausted data -> printf "%sArguments exhausted after %i tests%s" name data.NumberOfTests (data.Stamps |> stamps_to_string )
    }
       
let check config property = runner config property

let quick = { maxTest = 100
              maxFail = 1000
              name    = ""
              size    = fun prevSize -> prevSize + 0.5
              every   = fun ntest args -> "" 
              runner  = consoleRunner } 
         
let verbose = 
    { quick with every = fun n args -> any_to_string n + ":\n" + (List.fold_left (fun b a -> a + "\n" + b) "" args)  }

let quickCheck p = p |> check quick
let verboseCheck p = p |> check verbose
let qcheck gen p = forAll gen p |> quickCheck
let vcheck gen p = forAll gen p |> verboseCheck

//parametrized active pattern that recognizes generic types with generic type definitions equal to the first paramater, 
//and that returns the generic type parameters of the generic type.
let (|GenericTypeDef|_|) (p:Type) (t:Type) = 
    try
        let generic = t.GetGenericTypeDefinition() 
        if p.Equals(generic) then Some(t.GetGenericArguments()) else None
    with _ -> None

let findGenerators = 
    let addMethods l (t:Type) =
        t.GetMethods((BindingFlags.Static ||| BindingFlags.Public)) |>
        Seq.fold (fun l m ->
            //let returnType = m.ReturnType
            let gen = typedefof<Gen<_>>
            match m.ReturnType with
                | GenericTypeDef gen args -> 
                    ((if args.[0].IsGenericType then args.[0].GetGenericTypeDefinition() else args.[0]), m) :: l  
                | _ -> l
            ) l
    addMethods []

let generators = new Dictionary<_,_>()

let registerGenerators t =
    findGenerators t |> Seq.iter (fun (t,mi) -> generators.Add(t, mi))

registerGenerators (typeof<Gen>)

let rec getGenerator (genericMap:IDictionary<_,_>) (t:Type)  =
    if t.IsGenericParameter then
        if genericMap.ContainsKey(t) then 
            genericMap.[t]
        else
            let newGenerator =  
                generators.Keys 
                |> Seq.filter (fun t -> not t.IsGenericType) 
                |> Seq.map (getGenerator genericMap)
                |> Seq.to_list
                |> elements
                |> generate 0 (newSeed())
            genericMap.Add(t, newGenerator)
            newGenerator
    else
        let t' = if t.IsGenericType then t.GetGenericTypeDefinition() else t
        let mi = generators.[t']
        let args = t.GetGenericArguments() |> Array.map (getGenerator genericMap)
        let typeargs = args |> Array.map (fun o -> o.GetType().GetGenericArguments().[0])
        let mi' = if mi.ContainsGenericParameters then mi.MakeGenericMethod(typeargs) else mi
        mi'.Invoke(null, args)

let rec resolve (a:Type) (f:Type) (acc:Dictionary<_,_>) =
    if f.IsGenericParameter then
        if not (acc.ContainsKey(f)) then acc.Add(f,a)
    else 
        Array.zip (a.GetGenericArguments()) (f.GetGenericArguments())
        |> Array.iter (fun (act,form) -> resolve act form acc)

let invokeMethod (m:MethodInfo) args =
    let m = if m.ContainsGenericParameters then
                let typeMap = new Dictionary<_,_>()
                Array.zip args (m.GetParameters()) 
                |> Array.iter (fun (a,f) -> 
                    resolve (a.GetType()) f.ParameterType typeMap)  
                let actuals = 
                    m.GetGenericArguments() 
                    |> Array.map (fun formal -> typeMap.[formal])
                m.MakeGenericMethod(actuals)
            else 
                m
    m.Invoke(null, args)


let qcheckType config (t:Type) = 
    t.GetMethods((BindingFlags.Static ||| BindingFlags.Public)) |>
    Array.map(fun m -> 
        let genericMap = new Dictionary<_,_>()
        //this needs IGen cause can't cast Gen<anything> to Gen<obj> directly (no variance!)
        let gen = m.GetParameters() 
                    |> Array.map(fun p -> (getGenerator genericMap p.ParameterType  :?> IGen).AsGenObject )
                    |> Array.to_list
                    |> sequence
                    |> (fun gen -> gen.Map List.to_array)
        check {config with name = t.Name+"."+m.Name} (forAll gen (fun g -> invokeMethod m g |> unbox<Property> ))) |>
    ignore
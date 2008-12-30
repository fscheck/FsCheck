#light

namespace FSCheck

open System
open System.Collections.Generic
open Microsoft.FSharp.Compatibility.Lazy

open FSCheck.Random

module FSCheck = 

    type Gen<'a> = Gen of (int -> StdGen -> 'a)
                    with    
                    member x.Map f = match x with
                                        (Gen g) -> Gen (fun n r -> f <| g n r)

    type Ser<'a> = Ser of (int -> seq<'a>)

    type GenBuilder () =
        member b.Return(a) : Gen<_> = 
            Gen (fun n r -> a)
        member b.Bind((Gen m) : Gen<_>, k : _ -> Gen<_>) : Gen<_> = 
            Gen (fun n r0 -> let r1,r2 = split r0
                             let (Gen m') = k (m n r1) 
                             m' n r2)                                      
        member b.Let(p, rest) : Gen<_> = rest p
        //not so sure about this one...
        member b.Delay(f : unit -> Gen<_>) : Gen<_> = f () 

    let sized fgen = Gen (fun n r -> let (Gen m) = fgen n in m n r)

    let resize n (Gen m) = Gen (fun _ r -> m n r)

    let rand = Gen (fun n r -> r)

    //generates a value out of the generator with maximum size n
    let generate n rnd (Gen m) = 
        let size,rnd' = range (0,n) rnd
        m size rnd'

    let gen = GenBuilder()

    //haskell generates within closed interval [l,h], FsCheck generates
    //in half-open interval [l,h[
    //haskell types are also more general; this always results in an int 
    let choose (l, h) = rand.Map (range (l,h) >> fst) 
    //Gen ( fun n -> Gen.Random.Next(h) + l )

    let elements xs = (choose (0, List.length xs) ).Map(List.nth xs)

    let oneof gens = gen.Bind(elements gens, fun x -> x)

    let frequency (xs : list<int * Gen<'a>>) = 
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

    // let fractionPlus a b c = abs (fraction a  b c)

    let vector arbitrary n = 
        let rec sequence l = match l with
                                | [] -> gen { return [] }
                                | c::cs -> gen {let! x = c
                                                let! xs = sequence cs
                                                return  x::xs } 
        in
            sequence [ for i in 1..n -> arbitrary ]

    let array arbitrary n = vector arbitrary n |> liftGen Array.of_seq

    let promote f = Gen (fun n r -> fun a -> let (Gen m) = f a in m n r)

    let variant = fun v (Gen m) ->
        let rec rands r0 = seq { let r1,r2 = split r0 in yield! Seq.cons r1 (rands r2) } 
        Gen (fun n r -> m n (Seq.nth (v+1) (rands r)))

    type Gen =
        static member Unit = gen { return () }
        static member Bool = elements [true; false]
        static member Int = sized <| fun n -> choose (-n,n)
        static member Float = liftGen3 fraction Gen.Int Gen.Int Gen.Int
        static member Tuple(a,b) = liftGen2 (fun x y -> (x,y)) a b
        static member Tuple(a,b,c) = liftGen3 (fun x y z -> (x,y,z)) a b c 
        static member Tuple(a,b,c,d) = liftGen4 (fun x y z w -> (x,y,z,w)) a b c d
        static member Option(a) = 
            let arbMaybe size = 
                match size with
                    | 0 -> gen { return None }
                    | n -> (a |> resize (n-1)).Map(Some) 
            in sized arbMaybe
        static member List(a) = sized (fun n -> gen.Bind(choose(0,n), vector a))
        static member Arrow(coa,genb) = promote (fun a -> coa a genb) 

    type Co =
        static member Unit a = variant 0
        static member Bool b = if b then variant 0 else variant 1
        static member Int n = variant (if n >= 0 then 2*n else 2*(-n) + 1)
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
        static member Arrow gena cob f gn = 
            gen { let! x = gena
                  return! cob (f x) gn }

    type Result = { ok : option<Lazy<bool>>
                    stamp : list<string>
                    arguments : Lazy<list<string>> }

    let nothing = { ok = None; stamp = []; arguments = lazy [] }

    //testable is removed: it served no purpose here
    //the only instance of Testable was Property. Better use the Property directly then...
    //also it hampered inclusion of lazyness
    (*type Testable = 
        abstract property: Property*)
    type Property = Prop of Gen<Result>
                    (*interface Testable with
                        member x.property = x*)

    let result res = gen { return res } |> Prop

                           
    let evaluate prop = match prop with (Prop gen) -> gen

    let forAll gn body = 
        let argument a res = { res with arguments = lazy (any_to_string a :: res.arguments.Value) } in
        Prop <|  gen { let! a = gn
                       let! res = (evaluate (body a))
                       return (argument a res) }

    let emptyProperty = result nothing
        (*{ new Testable with
            member x.property =  }*)

    let implies b a = if b then a else emptyProperty
    let (==>) b a = implies b a

    let label str a = 
        let add res = { res with stamp = str :: res.stamp } in
        Prop ((evaluate a).Map add)

    let classify b name a = if b then label name a else a

    let trivial b = classify b "trivial"

    let collect v = label <| any_to_string v

    let prop b = gen { return {nothing with ok = Some b}} |> Prop
    let propl b = gen { return {nothing with ok = Some (b |> Lazy.CreateFromValue)}} |> Prop

    type Config = 
        { maxTest : int
          maxFail : int
          size    : int -> int
          every   : int -> Lazy<string list> -> string }

    let quick = { maxTest = 100
                  maxFail = 1000
                  size    = fun x -> (x / 2) + 3
                  every   = fun n args -> "" } //TODO
             
    let verbose = 
        { quick with every = fun n args -> any_to_string n + ":\n" + (List.fold_left (fun b a -> a + "\n" + b) "" args.Value)  }

    let defaultConfig = quick

    let testsDone mesg (ntest:int) stamps =
        let display l = match l with
                            | []  -> ".\n"
                            | [x] -> " (" + x + ").\n"
                            | xs  -> ".\n" + List.fold_left (fun acc x -> x + ".\n"+ acc) "" xs
        let percentage n m = any_to_string ((100 * n) / m) + "%"
        let rec intersperse sep l = match l with
                                    | [] -> []
                                    | [x] -> [x]
                                    | x::xs -> x :: sep :: intersperse sep xs  
        let entry (n,xs) = (percentage n ntest) + " " + (intersperse ", " xs |> Seq.to_array |> String.Concat)
        let table = stamps 
                    |> Seq.filter (fun l -> l <> []) 
                    |> Seq.sort_by (fun x -> x) 
                    |> Seq.group_by (fun x -> x) 
                    |> Seq.map (fun (l, ls) -> (Seq.length ls, l))
                    |> Seq.sort_by (fun (l, ls) -> l)
                    |> Seq.map entry
                    |> Seq.to_list
                    |> display
        in Console.Write(mesg + " " + any_to_string ntest + " tests" + table:string)

    let rec tests config rnd0 gen ntest nfail stamps =
        if ntest = config.maxTest then testsDone "Ok, passed" ntest stamps
        else if nfail = config.maxFail then testsDone "Arguments exhausted after" ntest stamps
        else
            let rnd1, rnd2 = split rnd0
            let result = generate (config.size ntest) rnd2 gen in
            Console.Write(config.every ntest result.arguments);
            match result.ok with
                | None -> tests config rnd1 gen ntest (nfail+1) stamps
                | Some lazyBool -> if lazyBool.Value 
                                   then tests config rnd1 gen (ntest+1) nfail (result.stamp :: stamps)
                                   else Console.WriteLine( "Falsifiable, after " + any_to_string ntest + " tests:\n" + String.Join("\n", Array.of_list result.arguments.Value));
                
            
    let check config a = tests config (newSeed()) (evaluate a) 0 0 []

    let quickCheck p = p |> check quick
    let verboseCheck p = p |> check verbose
    let qcheck gen p = forAll gen p |> quickCheck
    let vcheck gen p = forAll gen p |> verboseCheck

    (*let fromLazy lazyPred a = (lazyPred a) |> prop
    let fromPred pred a = (pred a) |> lazy_from_val |> prop

    type Fs =
        [<OverloadID("1")>]
        static member ForAll(generator, property) = 
            forAll generator property
        [<OverloadID("2")>]
        static member ForAll(generator, lazyPredicate) = 
            forAll generator (fromLazy lazyPredicate)//(fun a -> (lazyPredicate a) |> prop)
        [<OverloadID("3")>]
        static member ForAll(generator, predicate) = 
            forAll generator (fromPred predicate)//(fun a -> (predicate a) |> lazy_from_val |> prop)
        [<OverloadID("1")>]
        static member Filter(condition, property) = 
            condition ==> property
        [<OverloadID("2")>]
        static member Filter(condition, lazyPredicate) = 
            condition ==> prop lazyPredicate
        static member Check property = quickCheck property
        static member CheckVerbose property = verboseCheck property
        [<OverloadID("1")>]
        static member CheckGen(generator, property) = 
            checkGen generator property
        [<OverloadID("2")>]
        static member CheckGen(generator, lazyPredicate) = 
            checkGen generator (fromLazy lazyPredicate)
        [<OverloadID("3")>]
        static member CheckGen(generator, predicate) = 
            checkGen generator (fromPred predicate)*)

    //problems with this overload approach:
    //- ugly
    //- combination properties of functions are lost due to mandatory tuple
    //- no infix possible
    //+ prop is superfluous
    //+ lazy is sometimes optional
    //--> member overload is out
#light

#r "bin\Debug\FSCheck.dll"

open System

open FSCheck.FSCheck
open FSCheck.GenReflect
open FSCheck.Shrink

let nowGeneflect (x : Gen<'a>) : Gen<'a> = geneflect ()

let prop_RevRev =     
    forAll (nowGeneflect <| Gen.List(Gen.Int)) (fun xs -> List.rev(List.rev xs) = xs |> propl)    
    
let runRevRev () = quickCheck prop_RevRev 

//or you may prefer this style
//advantages: property is more readable, generators are swapped more easily
let prop_RevRev' (xs:list<int>) = List.rev (List.rev xs) = xs  |> propl  

let runRevRev' () = qcheck (nowGeneflect <| Gen.List(Gen.Int)) prop_RevRev'    

 
let prop_RevId = forAll (nowGeneflect <| Gen.List(Gen.Int)) 
                    (fun xs -> List.rev xs = xs |> propl)
                    
let runRevId () = quickCheck prop_RevId


//other style
let prop_RevId' xs = List.rev xs = xs |> propl

let runRevId' () = qcheck (Gen.List(Gen.Int)) prop_RevId'

//helper functions
let rec ordered xs = match xs with
                     | [] -> true
                     | [x] -> true
                     | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec insert x xs = match xs with
                      | [] -> [x]
                      | c::cs -> if x<=c then x::xs else c::(insert x cs)
let prop_Insert = forAll (Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) 
                    (fun (x,xs) -> 
                    ordered xs ==> prop (lazy (ordered (insert x xs)))
                    |> trivial (List.length xs = 0))
                    
let runInsert () = quickCheck prop_Insert

//other style
let prop_Insert' (x,xs) =  
    ordered xs ==> prop (lazy (ordered (insert x xs)))
    |> trivial (List.length xs = 0)    
    
let runInsert' () = qcheck (nowGeneflect <| Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) prop_Insert'


let prop_Insert2 = 
    forAll (nowGeneflect <| Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) (fun (x,xs) -> 
        ordered xs ==> prop( lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail") 

let prop_Insert2' (x,xs) = 
        ordered xs ==> prop( lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        
let runInsert2' () = qcheck (nowGeneflect <| Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) prop_Insert2'

let prop_Insert3 = 
    forAll (nowGeneflect <| Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) (fun (x,xs) -> 
        ordered xs ==> prop (lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs))
            
let prop_Insert3' (x,xs) =
        ordered xs ==> prop (lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs)    
        
let runInsert3' () = qcheck (nowGeneflect <| Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) prop_Insert3'


//custom generators
type Tree = Leaf of int | Branch of Tree * Tree

(*let rec unsafeTree = 
    oneof [ liftGen (Leaf) Gen.Int; 
            liftGen2 (fun x y -> Branch (x,y)) unsafeTree unsafeTree]*)

let tree =
    let rec tree' s = 
        match s with
          | 0 -> liftGen (Leaf) Gen.Int
          | n when n>0 -> 
            let subtree = tree' (n/2) in
            oneof [ liftGen (Leaf) Gen.Int; 
                    liftGen2 (fun x y -> Branch (x,y)) subtree subtree]
          | _ -> raise (ArgumentException("Bug in tree function"))
    sized tree'
let rec cotree t = 
    match t with
       | (Leaf n) -> variant 0 << Co.Int n
       | (Branch (t1,t2)) -> variant 1 << cotree t1 << cotree t2


//let eq f g x = (f x) = (g x) 
let treegen = nowGeneflect <| Gen.Tuple(tree,three (Gen.Arrow(cotree,tree)))


let prop_Assoc (x,(f,g,h)) = ((f >> g) >> h) x = (f >> (g >> h)) x |> propl

let runAssoc () = qcheck (Gen.Tuple(Gen.Int, Gen.Arrow(Co.Int, Gen.Int) |> three)) prop_Assoc

let prop_RevUnit = forAll (nowGeneflect Gen.Int) (fun x -> List.rev [x] = [x] |> propl)

let runRuvUnit () = quickCheck prop_RevUnit

let prop_RevApp = forAll (nowGeneflect <| Gen.Tuple(Gen.Int,Gen.List(Gen.Int)))(fun (x,xs) -> 
                    List.rev (x::xs) = List.rev xs @ [x] 
                        |> propl
                        |> trivial (xs = [])
                        |> trivial (xs.Length = 1))
let runRevApp () = quickCheck prop_RevApp

let prop_MaxLe = forAll (nowGeneflect <| Gen.Tuple(Gen.Int, Gen.Int)) (fun (x,y) ->  
                    (x <= y) ==> prop (lazy (max  x y = y)))

let runMaxLe () = quickCheck prop_MaxLe


let chooseFromList xs = gen { let! i = choose (0, List.length xs) 
                              return (List.nth xs i) }

let chooseBool = oneof [ gen { return true }; gen { return false } ]
let chooseBool2 = frequency [ (2, gen { return true }); (1, gen { return false })]
let exampleSize = sized <| fun s -> choose (0,s)
let matrix gn = sized <| fun s -> resize (s|>float|>sqrt|>int) gn
(*let arr = Gen.Arrow(Co.Int, Gen.Float)
let f = generate 10 1234512345.0 arr
print_any [(f 0);(f 0);(f 1);(f 1);(f 2);(f 2);(f 3);(f 4)]*)
//read_line()


let prop_CheckLazy = forAll (Gen.Int) (fun a -> false ==> prop (lazy (Console.WriteLine("boom"); a = 0)))

let runCheckLazy () = quickCheck prop_CheckLazy

let prop_CheckLazy2 = forAll (Gen.Int) (fun a -> a <> 0 ==> prop (lazy (1/a = 1/a)))

type List<'a> = {list : 'a[]}

type Tree<'a> = 
    | Leaf of string
    | Branch of List<Tree<'a>>

let rec xmlSafeTree (x : Tree<string>) =
    match x with
    | Leaf x -> not (x.StartsWith " " && x.EndsWith " ")
    | Branch xs -> Array.for_all xmlSafeTree xs.list

let runXmlSafeTree () = quickCheck' <| fun x -> propl <| xmlSafeTree x


let runAnonymous () = quickCheck' <| fun (x:int,y:int) -> (x > 0 && y > 0) ==> propl (x*y > 0)

let revStr (x : string) =
    let cs = x.ToCharArray()
    Array.Reverse cs
    new String(cs)

let runRevStr () = quickCheck' <| fun x -> propl (x = revStr x)

let idempotent f x = let y = f x in f y = y

let runIdempotont () = quickCheck' (propl << idempotent (fun (x : string) -> x.ToUpper()))

(*
    printfn "Finished"

    Console.ReadKey() |> ignore
*)

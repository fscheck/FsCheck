#light

open FsCheck
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic;

type Properties =
    static member Test1 b = propl (b = true)
    static member Test2 i = propl (i < 100)
    static member Test3 (i,j) = propl (i < 10 && j < 5.1)
    static member Test4 l = propl ( List.rev l = l)
    static member Test5 (l:list<float>) = propl ( List.rev l = l)
    static member Test6 (l:list<list<int*int> * float>) = propl ((l |> List.rev |> List.rev) = l) |> trivial (List.length l = 0)
    static member Test7 (a,b) = propl (fst a = snd b)
    static member Test8 (l:list<obj>) = propl ( List.rev l = l)
    static member Test9 (s:string) = propl ( new String(s.ToCharArray()) = s )
    static member Test10 i = propl (i = 'r')
    //this gets compiled as a fastfunc
//    static member Test i j = propl (i < 100 && j < 100)

quickCheck (typeof<Properties>)

verboseCheck (fun (s:string) -> propl (s.Length <100))

//long version: define your own generator (constraint to list<int>)
let prop_RevRev =     
    forAll (Gen.List(Gen.Int)) (fun xs -> List.rev(List.rev xs) = xs |> propl)    
quickCheck prop_RevRev 

//short version, also 'truly' generic (i.e. will get lists of ints, chars, floats,...)
let sprop_RevRev xs = List.rev(List.rev xs) = xs    
quickCheck sprop_RevRev 

//obsolote this style?
//advantages:  generators are swapped more easily (are they? forall already does this...)
let prop_RevRev' (xs:list<int>) = List.rev (List.rev xs) = xs  |> propl  
qcheck (Gen.List(Gen.Int)) prop_RevRev'    

 

let prop_RevId = forAll (Gen.List(Gen.Int)) 
                    (fun xs -> List.rev xs = xs |> propl)
quickCheck prop_RevId

//other style
let prop_RevId' xs = List.rev xs = xs |> propl
qcheck (Gen.List(Gen.Int)) prop_RevId'

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
quickCheck prop_Insert

//other style
let prop_Insert' (x,xs) =  
    ordered xs ==> prop (lazy (ordered (insert x xs)))
    |> trivial (List.length xs = 0)    
qcheck (Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) prop_Insert'


let prop_Insert2 = 
    forAll (Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) (fun (x,xs) -> 
        ordered xs ==> prop( lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail") 

let prop_Insert2' (x,xs) = 
        ordered xs ==> prop( lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
qcheck (Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) prop_Insert2'

let prop_Insert3 = 
    forAll (Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) (fun (x,xs) -> 
        ordered xs ==> prop (lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs))
            
let prop_Insert3' (x,xs) =
        ordered xs ==> prop (lazy (ordered (insert x xs)))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs)    
qcheck (Gen.Tuple(Gen.Int, Gen.List(Gen.Int))) prop_Insert3'

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
            | _ -> raise(ArgumentException"Only positive arguments are allowed")
    sized tree'
let rec cotree t = 
    match t with
       | (Leaf n) -> variant 0 << Co.Int n
       | (Branch (t1,t2)) -> variant 1 << cotree t1 << cotree t2


let eq f g x = (f x) = (g x) 
let treegen = Gen.Tuple(tree,three (Gen.Arrow(cotree,tree)))

let prop_Assoc (x,(f,g,h)) = ((f >> g) >> h) x = (f >> (g >> h)) x |> propl
qcheck (Gen.Tuple(Gen.Int, Gen.Arrow(Co.Int, Gen.Int) |> three)) prop_Assoc


let prop_RevUnit = forAll Gen.Int (fun x -> List.rev [x] = [x] |> propl)
quickCheck prop_RevUnit

let prop_RevApp = forAll (Gen.Tuple(Gen.Int,Gen.List(Gen.Int)))(fun (x,xs) -> 
                    List.rev (x::xs) = List.rev xs @ [x] 
                        |> propl
                        |> trivial (xs = [])
                        |> trivial (xs.Length = 1))
quickCheck prop_RevApp

let prop_MaxLe = forAll (Gen.Tuple(Gen.Int, Gen.Int)) (fun (x,y) ->  
                    (x <= y) ==> prop (lazy (max  x y = y)))
quickCheck prop_MaxLe


let chooseFromList xs = gen { let! i = choose (0, List.length xs) 
                              return (List.nth xs i) }

let chooseBool = oneof [ gen { return true }; gen { return false } ]
let chooseBool2 = frequency [ (2, gen { return true }); (1, gen { return false })]
let exampleSize = sized <| fun s -> choose (0,s)
let matrix gn = sized <| fun s -> resize (s|>float|>sqrt|>int) gn

(*let arr = Gen.Arrow(Co.Int, Gen.Float)
let f = generate 10 1234512345.0 arr
print_any [(f 0);(f 0);(f 1);(f 1);(f 2);(f 2);(f 3);(f 4)]
read_line()*)

let prop_CheckLazy = forAll (Gen.Int) (fun a -> false ==> prop (lazy (Console.WriteLine("boom"); a = 0)))

let prop_CheckLazy2 = forAll (Gen.Int) (fun a -> a <> 0 ==> prop (lazy (1/a = 1/a)))

//let arr = Gen.Arrow(Co.Float,Gen.Arrow(Co.Int,Gen.Bool))
//let arr2 = Gen.Arrow(Co.Arrow (Gen.Float,Co.Int),Gen.Bool) //fun i -> 10.5) Gen.Int

Console.ReadKey() |> ignore
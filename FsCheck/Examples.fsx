#light

//strangely, the fsx file reports error, although it runs fine

#r "FSharp.Powerpack.dll"
#load "Random.fs" "Generator.fs" "Property.fs" "Runner.fs"

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic
open FsCheck

//-------A Simple Example----------
//short version, also polymorphic (i.e. will get lists of bools, chars,...)
let prop_RevRev xs = List.rev(List.rev xs) = xs

//long version: define your own generator (constrain to list<char>)
let lprop_RevRev =     
    forAll (Gen.List(Gen.Char)) (fun xs -> List.rev(List.rev xs) = xs |> propl)     

let prop_RevId xs = List.rev xs = xs

//------Grouping properties--------
type ListProperties =
    static member RevRev xs = prop_RevRev xs
    static member RevId xs = prop_RevId xs
quickCheck (typeof<ListProperties>)


//-----Properties----------------
let prop_RevRevFloat (xs:list<float>) = List.rev(List.rev xs) = xs

//Conditional Properties
let rec private ordered xs = match xs with
                             | [] -> true
                             | [x] -> true
                             | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec insert x xs = match xs with
                      | [] -> [x]
                      | c::cs -> if x <= c then x::xs else c::(insert x cs)                      
let prop_Insert (x:int) xs = ordered xs ==> propl (ordered (insert x xs))

//Lazy properties
let prop_Eager a = a <> 0 ==> propl (1/a = 1/a)
let prop_Lazy a = a <> 0 ==> prop (lazy (1/a = 1/a))

//Counting trivial cases
let prop_InsertTrivial (x:int) xs = 
    ordered xs ==> propl (ordered (insert x xs))
    |> trivial (List.length xs = 0)

//Classifying test cases
let prop_InsertClassify (x:int) xs = 
    ordered xs ==> propl (ordered (insert x xs))
    |> classify (ordered (x::xs)) "at-head"
    |> classify (ordered (xs @ [x])) "at-tail" 
    
//Collecting data values
let prop_InsertCollect (x:int) xs = 
    ordered xs ==> propl (ordered (insert x xs))
        |> collect (List.length xs)

//Combining observations
let prop_InsertCombined (x:int) xs = 
    ordered xs ==> propl (ordered (insert x xs))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs)

//--------Test Data Generators----------
let chooseFromList xs = gen { let! i = choose (0, List.length xs-1) 
                              return (List.nth xs i) }
//to generate a value out of a generator:
//generate <size> <seed> <generator>
//generate 0 (Random.newSeed()) (chooseFromList [1;2;3])

//Choosing between alternatives
let chooseBool = 
    oneof [ gen { return true }; gen { return false } ]
    
let chooseBool2 = 
    frequency [ (2, gen { return true }); (1, gen { return false })]

//The size of test data
let matrix gen = sized <| fun s -> resize (s|>float|>sqrt|>int) gen

//Generating Recusrive data types
type Tree = Leaf of int | Branch of Tree * Tree

let rec unsafeTree() = 
    oneof [ liftGen (Leaf) Gen.Int; 
            liftGen2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

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

//Generating functions
let rec cotree t = 
    match t with
       | (Leaf n) -> variant 0 << Co.Int n
       | (Branch (t1,t2)) -> variant 1 << cotree t1 << cotree t2

//Default generators by type

type Box<'a> = Whitebox of 'a | Blackbox of 'a

type MyGenerators =
    static member Tree = tree
    static member Box(gena) = 
        gen {   let! a = gena
                return! elements [ Whitebox a; Blackbox a] }
    
registerGenerators (typeof<MyGenerators>)

let prop_RevRevTree (xs:list<Tree>) = List.rev(List.rev xs) = xs

let prop_RevRevBox (xs:list<Box<int>>) = List.rev(List.rev xs) = xs |> propl |> collect xs

//----------Tips and tricks-------
//Testing functions
let treeGen = Gen.Tuple(tree,three (Gen.Arrow(cotree,tree)))

let prop_Assoc = 
    forAll treeGen (fun (x,(f,g,h)) ->
        ((f >> g) >> h) x = (f >> (g >> h)) x |> propl)

quickCheck (typeof<ListProperties>.DeclaringType)
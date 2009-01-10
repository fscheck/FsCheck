#light

open FsCheck
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic;
open FsCheck.TypeClass




//-------A Simple Example----------
//short version, also polymorphic (i.e. will get lists of bools, chars,...)
let prop_RevRev (xs:list<int>) = List.rev(List.rev xs) = xs
quickCheck prop_RevRev

//long version: define your own generator (constrain to list<char>)
//let lprop_RevRev =     
//    forAll (Gen.FsList(Gen.Char)) (fun xs -> List.rev(List.rev xs) = xs |> propl)     

let prop_RevId (xs:list<int>) = List.rev xs = xs
quickCheck prop_RevId

//------Grouping properties--------
type ListProperties =
    static member RevRev xs = prop_RevRev xs
    static member RevId xs = prop_RevId xs
//quickCheck (typeof<ListProperties>)


//-----Properties----------------
let prop_RevRevFloat (xs:list<float>) = List.rev(List.rev xs) = xs
quickCheck prop_RevRevFloat

//Conditional Properties
let rec private ordered xs = match xs with
                             | [] -> true
                             | [x] -> true
                             | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec insert x xs = match xs with
                      | [] -> [x]
                      | c::cs -> if x <= c then x::xs else c::(insert x cs)                      
let prop_Insert (x:int) xs = ordered xs ==> ordered (insert x xs)
quickCheck prop_Insert

//Lazy properties
let prop_Eager a = a <> 0 ==> (1/a = 1/a)
let prop_Lazy a = a <> 0 ==> (lazy (1/a = 1/a))
quickCheck prop_Eager
quickCheck prop_Lazy

//Counting trivial cases
let prop_InsertTrivial (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> trivial (List.length xs = 0)
quickCheck prop_InsertTrivial

//Classifying test cases
let prop_InsertClassify (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> classify (ordered (x::xs)) "at-head"
    |> classify (ordered (xs @ [x])) "at-tail" 
quickCheck prop_InsertClassify
    
//Collecting data values
let prop_InsertCollect (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> collect (List.length xs)
quickCheck prop_InsertCollect

//Combining observations
let prop_InsertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs)
quickCheck prop_InsertCombined

//--------Test Data Generators----------
let private chooseFromList xs = 
    gen {   let! i = choose (0, List.length xs-1) 
            return (List.nth xs i) }
//to generate a value out of a generator:
//generate <size> <seed> <generator>
//generate 0 (Random.newSeed()) (chooseFromList [1;2;3])

//Choosing between alternatives
let private chooseBool = 
    oneof [ gen { return true }; gen { return false } ]
    
let private chooseBool2 = 
    frequency [ (2, gen { return true }); (1, gen { return false })]

//The size of test data
let matrix gen = sized <| fun s -> resize (s|>float|>sqrt|>int) gen

//Generating Recusrive data types
type Tree = Leaf of int | Branch of Tree * Tree

let rec private unsafeTree() = 
    oneof [ liftGen (Leaf) arbitrary; 
            liftGen2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

let tree =
    let rec tree' s = 
        match s with
            | 0 -> liftGen (Leaf) arbitrary
            | n when n>0 -> 
            let subtree = tree' (n/2) in
            oneof [ liftGen (Leaf) arbitrary; 
                    liftGen2 (fun x y -> Branch (x,y)) subtree subtree]
            | _ -> raise(ArgumentException"Only positive arguments are allowed")
    sized tree'

//Generating functions
let rec cotree t = 
    match t with
       | (Leaf n) -> variant 0 << coarbitrary n
       | (Branch (t1,t2)) -> variant 1 << cotree t1 << cotree t2



//Default generators by type

type Box<'a> = Whitebox of 'a | Blackbox of 'a

let boxgen() = 
    gen {   let! a = arbitrary
            return! elements [ Whitebox a; Blackbox a] }

type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Arbitrary = tree
            override x.CoArbitrary t = cotree t }
    static member Box() = 
        {new Arbitrary<Box<'a>>() with
            override x.Arbitrary = boxgen() }

registerGenerators<MyGenerators>()

let prop_RevRevTree (xs:list<Tree>) = List.rev(List.rev xs) = xs
quickCheck prop_RevRevTree

let prop_RevRevBox (xs:list<Box<int>>) = List.rev(List.rev xs) = xs
quickCheck prop_RevRevBox

//----------Tips and tricks-------
//Testing functions
let prop_Assoc (x:Tree) (f:Tree->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x
quickCheck prop_Assoc

//-------------examples from QuickCheck paper-------------
let prop_RevUnit (x:char) = List.rev [x] = [x]

let prop_RevApp (x:string) xs = 
    List.rev (x::xs) = List.rev xs @ [x] 
        |> trivial (xs = [])
        |> trivial (xs.Length = 1)

let prop_MaxLe (x:float) y = (x <= y) ==> (lazy (max  x y = y))

//----------various examples

//let arr = Gen.Arrow(Co.Float,Gen.Arrow(Co.Int,Gen.Bool))
//let arr2 = Gen.Arrow(Co.Arrow (Gen.Float,Co.Int),Gen.Bool) //fun i -> 10.5) Gen.Int

//convoluted, absurd property, but shows the power of the combinators: it's no problem to return
//functions that return properties from your properties.
quickCheck (fun b y (x:char,z) -> if b then (fun q -> y+1 = z + int q) else (fun q -> q =10.0)) 

type Properties =
    static member Test1 (b,(b2:bool)) = (b = b2)
    static member Test2 i = (i < 100)
    static member Test3 (i,j) = (i < 10 && j < 5.1)
    //static member Test4 l = propl ( List.rev l = l) //generic args no longer work
    static member Test5 (l:list<float>) = List.rev l = l
    //this property is falsifiable: sometimes the generator for float generates nan; and nan <> nan
    //so when checking the reverse of the reverse list's equality with the original list, the check fails. 
    static member Test6 (l:list<list<int*int> * float>) = ((l |> List.rev |> List.rev) = l) |> trivial (List.length l = 0)
    static member Test7 (a:int*bool,b:float*int) = (fst a = snd b)
    //static member Test8 (l:list<obj>) = propl ( List.rev l = l) //no generator for objs yet
    static member Test9 (s:string) = ( new String(s.ToCharArray()) = s )
    static member Test10 i = (i = 'r')
    static member NoTest i = "30"

checkType quick (typeof<Properties>)

Console.WriteLine("----------Check all toplevel properties----------------");
type Marker = member x.Null = ()
//registerGenerators (typeof<Marker>.DeclaringType)
checkType quick (typeof<Marker>.DeclaringType)

//overwriteGenerators (typeof<MyGenerators>)

////------generators must take generic arguments-----------------------
//type Heap<'a> = Heap of list<'a>
//let insertH x (Heap xs) = Heap <| x::xs
//let empty = Heap []
//
//type WrongGen =
//    static member Heap = Gen.List(Gen.Int).Map (fun l -> List.fold_right insertH l empty)
//
//type RightGen =
//    static member Heap(elementGen) = (Gen.List(elementGen)).Map (fun l -> List.fold_right insertH l empty)
//
////registerGenerators(typeof<SpecificGen>) //--> will fail because of generator!
////let prop_Heap (h:Heap<int>) = true
////verboseCheck prop_Heap  

Console.ReadKey() |> ignore
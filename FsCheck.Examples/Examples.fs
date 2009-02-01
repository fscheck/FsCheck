#light

open FsCheck
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic;

//----bug involving recursive record types----------
type A = { A : A }
let private prop1 (a : A) = true

let rec a : A = { A = a } 
let private prop2() = forAll (elements [a]) prop1
printfn "prop1 works: %b" (prop1 a)
quickCheck prop2
quickCheck (fun () -> forAll (elements [a]) prop1)

//-------A Simple Example----------
let prop_RevRev (xs:list<int>) = List.rev(List.rev xs) = xs
quickCheck prop_RevRev

let prop_RevId (xs:list<int>) = List.rev xs = xs
quickCheckN "RevId" prop_RevId

//------Grouping properties--------
type ListProperties =
    static member RevRev xs = prop_RevRev xs
    static member RevId xs = prop_RevId xs
quickCheckAll (typeof<ListProperties>)

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
            let subtree() = tree' (n/2)
            oneof [ liftGen (Leaf) arbitrary; 
                    liftGen2 (fun x y -> Branch (x,y)) (subtree()) (subtree())]
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

//---GenReflect tests---
//a record type containing an array type
type List<'a> = {list : 'a[]}

//a recursive union type containing a record type
type Tree<'a> = 
    | Leaf of string
    | Branch of List<Tree<'a>>

let rec prop_xmlSafeTree (x : Tree<string>) =
    match x with
    | Leaf x -> not (x.StartsWith " " && x.EndsWith " ")
    | Branch xs -> Array.for_all prop_xmlSafeTree xs.list

let prop_Product (x:int,y:int) = (x > 0 && y > 0) ==> (x*y > 0)

let RevString (x : string) =
    let cs = x.ToCharArray()
    Array.Reverse cs
    new String(cs)

let prop_revstr x = RevString (RevString x) = x

let private idempotent f x = let y = f x in f y = y
quickCheck (idempotent (fun (x : string) -> x.ToUpper()))


//-------------examples from QuickCheck paper-------------
let prop_RevUnit (x:char) = List.rev [x] = [x]

let prop_RevApp (x:string) xs = 
    List.rev (x::xs) = List.rev xs @ [x] 
        |> trivial (xs = [])
        |> trivial (xs.Length = 1)

let prop_MaxLe (x:float) y = (x <= y) ==> (lazy (max  x y = y))

//----------various examples-------------------------------

//convoluted, absurd property, but shows the power of the combinators: it's no problem to return
//functions that return properties.
quickCheck (fun b y (x:char,z) -> if b then (fun q -> y+1 = z + int q) else (fun q -> q =10.0)) 

quickCheck (fun (arr:int[]) -> Array.rev arr = arr)

type ARecord = { XPos : int; YPos : int; Name: string }

quickCheck (fun (record:ARecord) -> (record.XPos > 0 && record.YPos > 0) ==> lazy (record.XPos * record.YPos > 0))

quickCheck (fun (a:int,b,c,d:int,e,f) (g,h,i) -> a > b && b > c && d > e && f > g && e > f && h > i && a > i)

type ADisc = 
    | First of int 
    | Second of char
    | Third of ADisc
    | Fourth of ADisc[]
    
quickCheck (fun (d:ADisc) -> match d with First i -> i = 2 | Second c -> true | Third _ -> true)

type Properties =
    static member Test1 (b,(b2:bool)) = (b = b2)
    static member Test2 i = (i < 100)
    static member Test3 (i,j) = (i < 10 && j < 5.1)
    //static member Test4 l =  List.rev l = l //generic args no longer work in quickCheckAll
    static member Test5 (l:list<float>) = List.rev l = l
    //this property is falsifiable: sometimes the generator for float generates nan; and nan <> nan
    //so when checking the reverse of the reverse list's equality with the original list, the check fails. 
    static member Test6 (l:list<list<int*int> * float>) = ((l |> List.rev |> List.rev) = l) |> trivial (List.length l = 0)
    static member Test7 (a:int*bool,b:float*int) = (fst a = snd b)
    static member Test8 (l:list<obj>) = ( List.rev l = l)
    static member Test9 (s:string) = ( new String(s.ToCharArray()) = s )
    static member Test10 i = (i = 'r')
    static member NoTest i = "30"

checkAll quick (typeof<Properties>)

Console.WriteLine("----------Check all toplevel properties----------------");
type Marker = member x.Null = ()
overwriteGeneratorsByType (typeof<Marker>.DeclaringType)
quickCheckAll (typeof<Marker>.DeclaringType)

Console.ReadKey() |> ignore

#light

open FsCheck
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic;
open FsCheck.TypeClass

(*
type IArbitrary<'a> =
    abstract Arbitrary : list<'a>
    abstract Shrink : 'a -> list<'a>

let arbitrary<'a> = getInstance (typedefof<IArbitrary<_>>) (typeof<'a>) |> unbox<IArbitrary<'a>> |> (fun arb -> arb.Arbitrary)
let shrink<'a> = getInstance (typedefof<IArbitrary<_>>) (typeof<'a>) |> unbox<(IArbitrary<'a>)> |> (fun arb -> arb.Shrink)

newTypeClass<IArbitrary<_>>

type ArbitraryInstances() =
    static member Unit() = 
        { new IArbitrary<unit> with
            override x.Arbitrary = [()]
            override x.Shrink _ = [] }
    static member Bool() = 
        { new IArbitrary<bool> with
            override x.Arbitrary = [true;false]
            override x.Shrink _ = [true;false] }
    static member Option<'a>() = 
        { new IArbitrary<option<'a>> with
            override x.Arbitrary = [ for i in arbitrary -> Some i]
            override x.Shrink a = match a with None -> [] | Some v -> [ for i in shrink v -> Some i ] }
    static member ListInt() = 
        { new IArbitrary<list<int>> with
            override x.Arbitrary = [ for i in 1..10 -> [i;i+1]]
            override x.Shrink a = match a with (x::xs) -> [xs] | _ -> [] }
    static member List<'a>() =
        { new IArbitrary<list<'a>> with
            override x.Arbitrary = [ for i in arbitrary -> [i]]
            override x.Shrink a = match a with (x::xs) -> [xs] | _ -> [] }
    static member CatchAll<'any>() =
        { new IArbitrary<'any> with
            override x.Arbitrary = [ "1" |> box |> unbox<'any> ]
            override x.Shrink (_:'any) :'any list = [ "1" |> box |> unbox<'any>]   }

registerInstances<IArbitrary<_>,ArbitraryInstances>()

printfn "%A" <| List.map shrink (arbitrary<bool>)
printfn "%A" <| List.map shrink (arbitrary<option<bool>>)
printfn "%A" <| arbitrary<list<int>>
printfn "%A" <| List.map shrink (arbitrary<option<list<int>>>)
printfn "%A" <| List.map shrink (arbitrary<list<bool>>)

printfn "any %A" <| arbitrary<string>
//printfn "any %A" <| arbitrary<int>
*)

let prop_RevRev (xs:list<DateTime>) (ys:list<char>) = 
    xs <> [] ==> (List.rev(List.rev xs) = xs)
    |> collect ys
    |> trivial (xs.Length = 1)

quickCheck prop_RevRev

(*
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

let boxgen gena = 
    gen {   let! a = gena
            return! elements [ Whitebox a; Blackbox a] }

type MyGenerators =
    static member Tree = tree
    static member Box(gena) = boxgen gena
     
    
let prop_RevRevTree (xs:list<Tree>) = List.rev(List.rev xs) = xs

let prop_RevRevBox (xs:list<Box<int>>) = List.rev(List.rev xs) = xs

//----------Tips and tricks-------
//Testing functions
let private treeGen = Gen.Tuple(tree,three (Gen.Arrow(cotree,tree)))

let prop_Assoc = 
    forAll treeGen (fun (x,(f,g,h)) ->
        ((f >> g) >> h) x = (f >> (g >> h)) x |> propl)

//-------------examples from QuickCheck paper-------------
let prop_RevUnit (x:char) = List.rev [x] = [x]

let prop_RevApp (x:string) xs = 
    List.rev (x::xs) = List.rev xs @ [x] 
        |> propl
        |> trivial (xs = [])
        |> trivial (xs.Length = 1)

let prop_MaxLe (x:float) y = (x <= y) ==> prop (lazy (max  x y = y))

//----------various examples

//let arr = Gen.Arrow(Co.Float,Gen.Arrow(Co.Int,Gen.Bool))
//let arr2 = Gen.Arrow(Co.Arrow (Gen.Float,Co.Int),Gen.Bool) //fun i -> 10.5) Gen.Int

type Properties =
    static member Test1 b = propl (b = true)
    static member Test2 i = propl (i < 100)
    static member Test3 (i,j) = propl (i < 10 && j < 5.1)
    static member Test4 l = propl ( List.rev l = l)
    static member Test5 (l:list<float>) = propl ( List.rev l = l)
    //this property is falsifiable: sometimes the generator for float generates nan; and nan <> nan
    //so when checking the reverse of the reverse list's equality with the original list, the check fails. 
    static member Test6 (l:list<list<int*int> * float>) = propl ((l |> List.rev |> List.rev) = l) |> trivial (List.length l = 0)
    static member Test7 (a,b) = propl (fst a = snd b)
    static member Test8 (l:list<obj>) = propl ( List.rev l = l)
    static member Test9 (s:string) = propl ( new String(s.ToCharArray()) = s )
    static member Test10 i = propl (i = 'r')
    static member NoTest i = "30"

quickCheck (typeof<Properties>)

Console.WriteLine("----------Check all toplevel properties----------------");
type Marker = member x.Null = ()
registerGenerators (typeof<Marker>.DeclaringType)
quickCheck (typeof<Marker>.DeclaringType)

overwriteGenerators (typeof<MyGenerators>)



//------generators must take generic arguments-----------------------
type Heap<'a> = Heap of list<'a>
let insertH x (Heap xs) = Heap <| x::xs
let empty = Heap []

type WrongGen =
    static member Heap = Gen.List(Gen.Int).Map (fun l -> List.fold_right insertH l empty)

type RightGen =
    static member Heap(elementGen) = (Gen.List(elementGen)).Map (fun l -> List.fold_right insertH l empty)

//registerGenerators(typeof<SpecificGen>) //--> will fail because of generator!
//let prop_Heap (h:Heap<int>) = true
//verboseCheck prop_Heap  
*)
Console.ReadKey() |> ignore
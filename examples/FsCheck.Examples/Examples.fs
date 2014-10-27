
open FsCheck
open System
open System.Threading
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic

open Prop

//init bug
let sizedString =  Arb.generate<char> |> Gen.sample 10 10
    

//---too early initialization bug (put this first): fixed---
type Generators =
  static member Int64() =
    { new Arbitrary<int64>() with
        override x.Generator = Arb.generate<int> |> Gen.map int64 }
Arb.register<Generators>() |> ignore

//check that registering typeclass instances with a class that does not define any no longer fails silently
//type internal NoInstancesFails() =
//    static member Int64() =
//        { new Arbitrary<int64>() with
//            override x.Arbitrary = arbitrary |> fmapGen int64 }
//overwriteGenerators<NoInstancesFails>()

type TestEnum =
    | First = 0
    | Second = 1
    | Third = 2

let testEnum (e:TestEnum) = e = TestEnum.First
Check.Quick testEnum

//bug: exception escapes: fixed
let prop_EscapingException (x:int) =
    if x=0 then lazy (failwith "nul") else lazy true
    |> label "bla"
Check.Verbose prop_EscapingException

//more escaping exceptions?
let somefailingFunction() = failwith "escaped"

let prop_EscapingException2 (x:int) =
    x <> 0 ==> lazy (if x=0 then false else (somefailingFunction()))
Check.Quick prop_EscapingException2

//bug: label not printed because of exception. Workaround: use lazy.
//actually I don't hink this is fixable, as the exception rolls up the stack, so the labelling
//that happens when a property "returns" gets bypassed.
//this is irritating when using Assert statements from unit testing frameworks though.
let prop_LabelBug (x:int) =
    if x=0 then lazy (failwith "nul") else lazy true
    |> label "bla"
Check.Quick prop_LabelBug

//smart shrinking
[<StructuredFormatDisplay("{Display}")>]
type Smart<'a> = 
    Smart of int * 'a with
        override x.ToString() = match x with Smart (_,a) -> sprintf "%A" a
        member x.Display = x.ToString()
    

type SmartShrinker =
    static member Smart() =
        { new Arbitrary<Smart<'a>>() with
            override x.Generator = Arb.generate |> Gen.map (fun arb -> Smart (0,arb))
            override x.Shrinker (Smart (i,x)) = 
                let ys = Seq.zip {0..Int32.MaxValue} (Arb.shrink x) |> Seq.map Smart 
                let i' = Math.Max(0,i-2)
                let rec interleave left right =
                    match (left,right) with
                    | ([],rs) -> rs
                    | (ls,[]) -> ls
                    | (l::ls,r::rs) -> l::r::(interleave ls rs)
                interleave (Seq.take i' ys |> Seq.toList) (Seq.skip i' ys |> Seq.toList) |> List.toSeq
        }

Arb.register<SmartShrinker>() |> ignore

let smartShrink (Smart (_,i)) = i < 20
Check.Quick smartShrink

//-------------examples from QuickCheck paper-------------
let prop_RevUnit (x:char) = List.rev [x] = [x]

let prop_RevApp (x:string) xs = 
    List.rev (x::xs) = List.rev xs @ [x] 
        |> trivial (xs = [])
        |> trivial (xs.Length = 1)

let prop_MaxLe (x:float) y = (x <= y) ==> (lazy (max  x y = y))

//----------various examples-------------------------------

//convoluted property, but shows the power of the combinators: it's no problem to return
//functions that return properties.
Check.Quick (fun b y (x:char,z) -> if b then (fun q -> y+1 = z + int q) else (fun q -> q =10.0)) 

//arrays
let prop_RevRevArr (xs:int[]) = Array.rev(Array.rev xs) = xs
Check.Quick prop_RevRevArr

let prop_RevRevArr2 (xs:int[][]) = xs.Rank = 1
Check.Quick prop_RevRevArr2

Check.Quick (fun (arr:int[]) -> Array.rev arr = arr)

type ARecord = { XPos : int; YPos : int; Name: string }

Check.Quick (fun (record:ARecord) -> (record.XPos > 0 && record.YPos > 0) ==> lazy (record.XPos * record.YPos > 0))

Check.Quick (fun (a:int,b,c,d:int,e,f) (g,h,i) -> a > b && b > c && d > e && f > g && e > f && h > i && a > i)

type ADisc = 
    | First of int 
    | Second of char
    | Third of ADisc
    | Fourth of ADisc[]
    
Check.Quick (fun (d:ADisc) -> match d with First i -> i = 2 | Second c -> true | Third _ -> true | Fourth _ -> raise <| InvalidOperationException())

type Properties =
    static member Test1 (b,(b2:bool)) = (b = b2)
    static member Test2 i = (i < 100)
    static member Test3 (i,j) = (i < 10 && j < 5.1)
    //static member Test4 l =  List.rev l = l //generic args no longer work in Check.QuickAll
    static member Test5 (l:list<float>) = List.rev l = l
    //this property is falsifiable: sometimes the generator for float generates nan; and nan <> nan
    //so when checking the reverse of the reverse list's equality with the original list, the check fails. 
    static member Test6 (l:list<list<int*int> * float>) = ((l |> List.rev |> List.rev) = l) |> trivial (List.length l = 0)
    static member Test7 (a:int*bool,b:float*int) = (fst a = snd b)
    static member Test8 (l:list<obj>) = ( List.rev l = l)
    static member Test9 (s:string) = ( new String(s.ToCharArray()) = s )
    static member Test10 i = (i = 'r')
    static member NoTest i = "30"
    static member OptionTest (o:option<int>) = match o with Some _ -> false | _ -> true

Check.QuickAll<Properties>()

//-----------ReflectArbitrary tests------------------------
//a record type containing an array type
type List<'a> = {list : 'a[]}

//a recursive union type containing a record type
type Tree<'a> = 
    | Leaf of string
    | Branch of List<Tree<'a>>

let rec xmlSafeTree (x : Tree<string>) =
    match x with
    | Leaf x -> not (x.StartsWith " " && x.EndsWith " ")
    | Branch xs -> Array.forall xmlSafeTree xs.list

let product (x:int,y:int) = (x > 0 && y > 0) ==> (x*y > 0)

let revString (x : string) =
    let cs = x.ToCharArray()
    Array.Reverse cs
    new String(cs)

let revRevString x = revString (revString x) = x

let private idempotent f x = let y = f x in f y = y
Check.Quick (idempotent (fun (x : string) -> x.ToUpper()))

let bigTuple (a:bool,b:float,c:string,d:char,e:byte,f:float,g:string,h:option<float>,i) = if i > 10 then false else true
Check.Quick("bigTuple",bigTuple)

//-----property combinators------------------
let private withPositiveInteger (p : int -> 'a) = fun n -> n <> 0 ==> lazy (p (abs n))

let testProp = withPositiveInteger ( fun x -> x > 0 |> classify true "bla"  )
Check.Quick testProp

let testProp2 = withPositiveInteger ( fun x -> withPositiveInteger (fun y -> x + y > 0  ))
Check.Quick testProp2

let blah (s:string) = if s = "" then raise (new System.Exception("foo")) else s.Length > 3

let private withNonEmptyString (p : string -> 'a) = forAll (Gen.elements [ "A"; "AA"; "AAA" ] |> Arb.fromGen) p

Check.Quick (withNonEmptyString blah)

let prop_Exc = forAll (Arb.fromGenShrink(Gen.resize 100 Arb.generate,Arb.shrink)) (fun (s:string) -> failwith "error")
Check.Quick("prop_Exc",prop_Exc)


//-----------------test reflective shrinking--------
type RecordStuff<'a> = { Yes:bool; Name:'a; NogIets:list<int*char> }

let bigSize = { Config.Quick with StartSize = 100; EndSize = 100 }

Check.One(bigSize,fun (s:RecordStuff<string>) -> s.Yes)

type Recursive<'a> = Void | Leaf of 'a | Branch of Recursive<'a> * 'a * Recursive<'a>

Check.One(bigSize,fun (s:Recursive<string>) -> match s with  Branch _ -> false | _ -> true)

type Simple = Void | Void2 | Void3 | Leaf of int | Leaf2 of string * int *char * float

//should yield a simplified Leaf2
Check.One(bigSize,fun (s:Simple) -> match s with Leaf2 _ -> false |  _ -> true)

//should yield a Void3
Check.One(bigSize,fun (s:Simple) -> match s with Leaf2 _ -> false | Void3 -> false |  _ -> true)

Check.One(bigSize,fun i -> (-10 < i && i < 0) || (0 < i) && (i < 10 ))
Check.Quick (fun opt -> match opt with None -> false | Some b  -> b  )
Check.Quick (fun opt -> match opt with Some n when n<0 -> false | Some n when n >= 0 -> true | _ -> true )

let prop_RevId' (xs:list<int>) (x:int) = if (xs.Length > 2) && (x >10) then false else true
Check.Quick prop_RevId'


//----------Checking toplevel properties trick------------------
Console.WriteLine("----------Check all toplevel properties----------------");
type Marker = class end

Check.QuickAll (typeof<Marker>.DeclaringType)

Console.ReadKey() |> ignore

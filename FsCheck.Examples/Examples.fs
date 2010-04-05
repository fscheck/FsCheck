
open FsCheck
open System
open System.Threading
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic

open Prop

//Check.One ({Config.Verbose with MaxTest = 10},fun (i:int) -> true)
//
//Console.ReadKey()

//---too early initialization bug (put this first): fixed---
type Generators =
  static member Int64() =
    { new Arbitrary<int64>() with
        override x.Arbitrary = Gen.arbitrary |> Gen.map int64 }
Gen.register<Generators>()

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

//-------A Simple Example----------
let prop_RevRev (xs:list<int>) = List.rev(List.rev xs) = xs
Check.Quick prop_RevRev

let prop_RevId (xs:list<int>) = List.rev xs = xs
Check.Quick("RevId",prop_RevId)

//------Grouping properties--------
type ListProperties =
    static member RevRev xs = prop_RevRev xs
    static member RevId xs = prop_RevId xs
Check.QuickAll<ListProperties>()




//-----Properties----------------
let prop_RevRevFloat (xs:list<float>) = List.rev(List.rev xs) = xs
Check.Quick prop_RevRevFloat

//Conditional Properties
let rec private ordered xs = match xs with
                             | [] -> true
                             | [x] -> true
                             | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec insert x xs = match xs with
                      | [] -> [x]
                      | c::cs -> if x <= c then x::xs else c::(insert x cs)                      
let prop_Insert (x:int) xs = ordered xs ==> ordered (insert x xs)
Check.Quick prop_Insert

//Lazy properties
let prop_Eager a = a <> 0 ==> (1/a = 1/a)
let prop_Lazy a = a <> 0 ==> (lazy (1/a = 1/a))
Check.Quick prop_Eager
Check.Quick prop_Lazy

//throws combinator
let prop_ExpectException() = throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))
Check.Quick prop_ExpectException

//within combinator
let prop_timeout (a:int) = 
    lazy
        if a>10 then
            while true do Thread.Sleep(1000)
            true
        else 
            true
    |> within 2000
Check.Quick prop_timeout

//Counting trivial cases
let prop_InsertTrivial (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> trivial (List.length xs = 0)
Check.Quick prop_InsertTrivial

//Classifying test cases
let prop_InsertClassify (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> classify (ordered (x::xs)) "at-head"
    |> classify (ordered (xs @ [x])) "at-tail" 
Check.Quick prop_InsertClassify
    
//Collecting data values
let prop_InsertCollect (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> collect (List.length xs)
Check.Quick prop_InsertCollect

//Combining observations
let prop_InsertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs)
Check.Quick prop_InsertCombined

//-------labelling sub properties------
let complexProp (m: int) (n: int) =
  let res = n + m
  (res >= m)    |@ "result > #1" .&.
  (res >= n)    |@ "result > #2" .&.
  (res < m + n) |@ "result not sum"
Check.Quick complexProp

let prop_Label (x:int) = 
    "Always false" @| false
    .&. "Always true" @| (x > 0 ==> (abs x - x = 0))
Check.Quick prop_Label

let propMul (n: int, m: int) =
  let res = n*m
  sprintf "evidence = %i" res @| (
    "div1" @| (m <> 0 ==> lazy (res / m = n)),
    "div2" @| (n <> 0 ==> lazy (res / n = m)),
    "lt1"  @| (res > m),
    "lt2"  @| (res > n))
Check.Quick propMul

let propMulList (n: int, m: int) =
  let res = n*m
  sprintf "evidence = %i" res @| [
    "div1" @| (m <> 0 ==> lazy (res / m = n));
    "div2" @| (n <> 0 ==> lazy (res / n = m));
    "lt1"  @| (res > m);
    "lt2"  @| (res > n)]
Check.Quick propMul

let propOr (n:int) (m:int) =
    let res = n - m
    "Positive"  @| (res > 0) .|.
    "Negative"  @| (res < 0) .|.
    "Zero"      @| (res = 0)
    |> classify (res > 0) "Positive"
    |> classify (res < 0) "Negative"   
Check.Quick propOr



//--------Test Data Generators----------
let private chooseFromList xs = 
    gen {   let! i = Gen.choose (0, List.length xs-1) 
            return (List.nth xs i) }

//to generate a value out of a generator:
//generate <size> <seed> <generator>
//generate 0 (Random.newSeed()) (chooseFromList [1;2;3])

//Choosing between alternatives
let private chooseBool = 
    Gen.oneof [ gen { return true }; gen { return false } ]
    
let private chooseBool2 = 
    Gen.frequency [ (2, gen { return true }); (1, gen { return false })]

//The size of test data
let matrix gen = Gen.sized <| fun s -> Gen.resize (s|>float|>sqrt|>int) gen

//Generating Recusrive data types
type Tree = Leaf of int | Branch of Tree * Tree

let rec private unsafeTree() = 
    Gen.oneof [ Gen.map Leaf Gen.arbitrary; 
                Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

let private tree =
    let rec tree' s = 
        match s with
            | 0 -> Gen.map Leaf Gen.arbitrary
            | n when n>0 -> 
            let subtree() = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Gen.arbitrary; 
                        Gen.map2 (fun x y -> Branch (x,y)) (subtree()) (subtree())]
            | _ -> raise(ArgumentException"Only positive arguments are allowed")
    Gen.sized tree'

//Generating functions
let rec private cotree t = 
    match t with
       | (Leaf n) -> Gen.variant 0 << Gen.coarbitrary n
       | (Branch (t1,t2)) -> Gen.variant 1 << cotree t1 << cotree t2


//Default generators by type
type Box<'a> = Whitebox of 'a | Blackbox of 'a

let boxgen() = 
    gen {   let! a = Gen.arbitrary
            return! Gen.elements [ Whitebox a; Blackbox a] }

type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Arbitrary = tree
            override x.CoArbitrary t = cotree t
            override x.Shrink t = Seq.empty }
    static member Box() = 
        {new Arbitrary<Box<'a>>() with
            override x.Arbitrary = boxgen() }

Gen.register<MyGenerators>()

let prop_RevRevTree (xs:list<Tree>) = List.rev(List.rev xs) = xs
Check.Quick prop_RevRevTree

let prop_RevRevBox (xs:list<Box<int>>) = List.rev(List.rev xs) = xs
Check.Quick prop_RevRevBox


//------Stateful Testing-----------
open Commands

type Counter() =
  let mutable n = 0
  member x.Inc() = n <- n + 1
  member x.Dec() = if n > 2 then n <- n - 2 else n <- n - 1
  member x.Get = n
  member x.Reset() = n <- 0
  override x.ToString() = n.ToString()

let spec =
    let inc = 
        { new ICommand<Counter,int>() with
            member x.RunActual c = c.Inc(); c
            member x.RunModel m = m + 1
            member x.Post (c,m) = m = c.Get 
            override x.ToString() = "inc"}
    let dec = 
        { new ICommand<Counter,int>() with
            member x.RunActual c = c.Dec(); c
            member x.RunModel m = m - 1
            member x.Post (c,m) = m = c.Get 
            override x.ToString() = "dec"}
    { new ISpecification<Counter,int> with
        member x.Initial() = (new Counter(),0)
        member x.GenCommand _ = Gen.elements [inc;dec] }

Check.Quick("Counter",asProperty spec)

//---------Replaying previous tests-----------

Check.One({ Config.Quick with Name="Counter-replay"; Replay = Some <| Random.StdGen (395461793,1) },asProperty spec)

//----------Tips and tricks-------
//Testing functions
let prop_Assoc (x:Tree) (f:Tree->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x
Check.Quick prop_Assoc

//Function printing and shrinking
let propMap (Function (_,f)) (l:list<int>) =
    not l.IsEmpty ==>
    lazy (List.map f l = ((*f*)(List.head l)) :: (List.map f (List.tail l)))
Check.Quick propMap

//alternative to using forAll
type NonNegativeInt = NonNegative of int
type NonZeroInt = NonZero of int
type PositiveInt = Positive of int

type ArbitraryModifiers =
    static member NonNegativeInt() =
        { new Arbitrary<NonNegativeInt>() with
            override x.Arbitrary = Gen.arbitrary |> Gen.map (NonNegative << abs)
            override x.CoArbitrary (NonNegative i) = Gen.coarbitrary i
            override x.Shrink (NonNegative i) = Gen.shrink i |> Seq.filter ((<) 0) |> Seq.map NonNegative }
    static member NonZeroInt() =
        { new Arbitrary<NonZeroInt>() with
            override x.Arbitrary = Gen.arbitrary |> Gen.suchThat ((<>) 0) |> Gen.map NonZero 
            override x.CoArbitrary (NonZero i) = Gen.coarbitrary i
            override x.Shrink (NonZero i) = Gen.shrink i |> Seq.filter ((=) 0) |> Seq.map NonZero }
    static member PositiveInt() =
        { new Arbitrary<PositiveInt>() with
            override x.Arbitrary = Gen.arbitrary |> Gen.suchThat ((<>) 0) |> Gen.map (Positive << abs) 
            override x.CoArbitrary (Positive i) = Gen.coarbitrary i
            override x.Shrink (Positive i) = Gen.shrink i |> Seq.filter ((<=) 0) |> Seq.map Positive }

Gen.register<ArbitraryModifiers>()

let prop_NonNeg (NonNegative i) = i >= 0
Check.Quick("NonNeg",prop_NonNeg)

let prop_NonZero (NonZero i) = i <> 0
Check.Quick("NonZero",prop_NonZero)

let prop_Positive (Positive i) = i > 0
Check.Quick("Pos",prop_Positive)

type Foo = Foo of int
type Bar = Bar of string

let formatter (o:obj) =
    match o with
    | :? Foo as foo -> box "it's a foo"
    | :? Bar as bar -> box "it's a bar"
    | _ -> o

//customizing output of counter-examples etc
let formatterRunner =
    { new IRunner with
        member x.OnArguments (ntest,args, every) =
            printf "%s" (every ntest (args |> List.map formatter))
        member x.OnShrink(args, everyShrink) =
            printf "%s" (everyShrink (args |> List.map formatter))
        member x.OnFinished(name,testResult) = 
            let testResult' = match testResult with 
                                | TestResult.False (testData,origArgs,shrunkArgs,outCome,seed) -> 
                                    TestResult.False (testData,origArgs |> List.map formatter, shrunkArgs |> List.map formatter,outCome,seed)
                                | t -> t
            printf "%s" (Runner.testFinishedToString name testResult') 
    }

let formatter_prop (foo:Foo) (bar:Bar) (i:int) = i < 10 //so it takes a while before the fail
Check.One({ Config.Quick with Runner = formatterRunner},formatter_prop)

let private (.=.) left right = left = right |@ sprintf "%A = %A" left right

let compare (i:int) (j:int) = 2*i+1  .=. 2*j-1
Check.Quick compare

//smart shrinking
[<StructuredFormatDisplay("{Display}")>]
type Smart<'a> = 
    Smart of int * 'a with
        override x.ToString() = match x with Smart (_,a) -> sprintf "%A" a
        member x.Display = x.ToString()
    

type SmartShrinker =
    static member Smart() =
        { new Arbitrary<Smart<'a>>() with
            override x.Arbitrary = Gen.arbitrary |> Gen.map (fun arb -> Smart (0,arb))
            override x.Shrink (Smart (i,x)) = //shrink i |> Seq.filter ((<) 0) |> Seq.map NonNegative 
                let ys = Seq.zip {0..Int32.MaxValue} (Gen.shrink x) |> Seq.map Smart 
                let i' = Math.Max(0,i-2)
                let rec interleave left right =
                    match (left,right) with
                    | ([],rs) -> rs
                    | (ls,[]) -> ls
                    | (l::ls,r::rs) -> l::r::(interleave ls rs)
                interleave (Seq.take i' ys |> Seq.toList) (Seq.skip i' ys |> Seq.toList) |> List.toSeq
        }

Gen.register<SmartShrinker>()

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

//convoluted, absurd property, but shows the power of the combinators: it's no problem to return
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

Check.QuickAll<Properties>()

//-----------GenReflect tests------------------------
//a record type containing an array type
type List<'a> = {list : 'a[]}

//a recursive union type containing a record type
type Tree<'a> = 
    | Leaf of string
    | Branch of List<Tree<'a>>

let rec prop_xmlSafeTree (x : Tree<string>) =
    match x with
    | Leaf x -> not (x.StartsWith " " && x.EndsWith " ")
    | Branch xs -> Array.forall prop_xmlSafeTree xs.list

let prop_Product (x:int,y:int) = (x > 0 && y > 0) ==> (x*y > 0)

let RevString (x : string) =
    let cs = x.ToCharArray()
    Array.Reverse cs
    new String(cs)

let prop_revstr x = RevString (RevString x) = x

let private idempotent f x = let y = f x in f y = y
Check.Quick (idempotent (fun (x : string) -> x.ToUpper()))


//-----property combinators------------------


let private withPositiveInteger (p : int -> 'a) = fun n -> n <> 0 ==> lazy (p (abs n))

let testProp = withPositiveInteger ( fun x -> x > 0 |> classify true "bla"  )
Check.Quick testProp

let testProp2 = withPositiveInteger ( fun x -> withPositiveInteger (fun y -> x + y > 0  ))
Check.Quick testProp2

let blah (s:string) = if s = "" then raise (new System.Exception("foo")) else s.Length > 3

let private withNonEmptyString (p : string -> 'a) = forAll (Gen.oneof (List.map gen.Return [ "A"; "AA"; "AAA" ])) p

Check.Quick (withNonEmptyString blah)

let prop_Exc = forAllShrink (Gen.resize 100 Gen.arbitrary) Gen.shrink (fun (s:string) -> failwith "error")
Check.Quick("prop_Exc",prop_Exc)


//-----------------test reflective shrinking--------

type RecordStuff<'a> = { Yes:bool; Name:'a; NogIets:list<int*char> }

Check.Quick <| 
    forAllShrink (Gen.resize 100 Gen.arbitrary) Gen.shrink (fun (s:RecordStuff<string>) -> s.Yes)

type Recursive<'a> = Void | Leaf of 'a | Branch of Recursive<'a> * 'a * Recursive<'a>

Check.Quick <| 
    forAllShrink (Gen.resize 100 Gen.arbitrary) Gen.shrink (fun (s:Recursive<string>) -> 
    match s with  Branch _ -> false | _ -> true)

type Simple = Void | Void2 | Void3 | Leaf of int | Leaf2 of string * int *char * float

//should yield a simplified Leaf2
Check.Quick <| 
    forAllShrink (Gen.resize 100 Gen.arbitrary) Gen.shrink (fun (s:Simple) -> 
    match s with Leaf2 _ -> false |  _ -> true)

//should yield a Void3
Check.Quick <| 
    forAllShrink (Gen.resize 100 Gen.arbitrary) Gen.shrink (fun (s:Simple) -> 
    match s with Leaf2 _ -> false | Void3 -> false |  _ -> true) 


Check.Quick <| forAllShrink (Gen.resize 100 Gen.arbitrary) Gen.shrink (fun i -> (-10 < i && i < 0) || (0 < i) && (i < 10 ))
Check.Quick (fun opt -> match opt with None -> false | Some b  -> b  )
Check.Quick (fun opt -> match opt with Some n when n<0 -> false | Some n when n >= 0 -> true | _ -> true )

let prop_RevId' (xs:list<int>) (x:int) = if (xs.Length > 2) && (x >10) then false else true
Check.Quick prop_RevId'


//----------Checking toplevel properties trick------------------
Console.WriteLine("----------Check all toplevel properties----------------");
type Marker = member x.Null = ()
//if there are instances defined: (throws exception if not)
//overwriteGeneratorsByType (typeof<Marker>.DeclaringType)
Check.QuickAll (typeof<Marker>.DeclaringType)

Console.ReadKey() |> ignore

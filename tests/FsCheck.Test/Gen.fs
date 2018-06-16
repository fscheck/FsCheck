﻿namespace FsCheck.Test

module Gen =

    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Helpers
    open Swensen.Unquote
    open System.Reflection

    [<Property>]
    let Choose (Interval (l,h)) = 
        Gen.choose (l,h)
        |> sample 10
        |> List.forall (fun v -> l <= v && v <= h)
    
    [<Property>] 
    let Elements (l:list<char>) =
        not l.IsEmpty ==> 
        lazy (  Gen.elements l
                |> sample 50
                |> List.forall (isIn l))

    [<Fact>]
    let GrowingElements () =
        let sizes = [ 1..100 ]
        let run g =
            [ for s in sizes -> Gen.resize s g ]
            |> Gen.sequence
            |> sample1

        let actual = Gen.growingElements sizes |> run

        test <@ List.forall2 (>=) sizes actual @>

    [<Property>]
    let constant (v : (int * char)) =
        Gen.constant v
        |> sample 10
        |> List.forall (fun actual -> obj.ReferenceEquals(actual, v))

    [<Property>]
    let fresh ((i:int,c:char) as v) =
        Gen.fresh (fun () -> (i,c)) 
        |> sample 10
        |> List.forall (fun actual -> not (obj.ReferenceEquals(actual, v)) && actual = v)
    
    [<Property>]
    let Oneof (l:list<string>) =
        not l.IsEmpty ==> 
        lazy (  List.map Gen.constant l
                |> Gen.oneof
                |> sample 10
                |> List.forall (isIn l))

    [<Property>]
    let Frequency (frequencies:list<PositiveInt*string>) =
        let generatedValues = frequencies |> List.filter (fst >> (fun p -> p.Get) >> (<>) 0) |> List.map snd
        (sprintf "%A" generatedValues) @|
        (not generatedValues.IsEmpty ==>
         lazy ( frequencies
                |> List.map (fun (freq,s) -> (freq.Get,Gen.constant s))
                |> Gen.frequency
                |> sample 100
                |> List.forall (isIn generatedValues)))

    [<Fact>]
    let ``frequency should throw argument exception if no element can be generated``() =
        let prop = lazy (Gen.frequency [(0,Gen.constant 1)]) //used lazy since unqoute couldn't handle generator directly
        raises<System.ArgumentException> <@ prop.Value @>
        
    
    [<Property>]
    let Map (f:string -> int) v =
        Gen.map f (Gen.constant v)
        |> sample 1
        |> List.forall ((=) (f v))
        
    [<Property>]
    let Map2 (f:char -> int -> int) a b =
        Gen.map2 f (Gen.constant a) (Gen.constant b)
        |> sample1
        |> ((=) (f a b))
        
    [<Property>]
    let Map3 (f:int -> char -> int -> int) a b c =
        Gen.map3 f (Gen.constant a) (Gen.constant b) (Gen.constant c)
        |> sample1
        |> ((=) (f a b c))
        
    [<Property>]
    let Map4 (f:char -> int -> char -> bool -> int) a b c d =
        Gen.map4 f (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d)
        |> sample1
        |> ((=) (f a b c d))
       
    [<Property>]
    let Map5 (f:bool -> char -> int -> char -> bool -> int) a b c d e =
        Gen.map5 f (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d) (Gen.constant e)
        |> sample1
        |> ((=) (f a b c d e))
    
    [<Property>]
    let Map6 (f:bool -> char -> int -> char -> bool -> int -> char ) a b c d e g = 
        Gen.map6 f (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d) (Gen.constant e) (Gen.constant g)
        |> sample1
        |> ((=) (f a b c d e g))

    [<Property>]
    let Zip (a : int) (b : char) =
        Gen.zip (Gen.constant a) (Gen.constant b)
        |> sample1
        |> ((=) (a, b))

    [<Property>]
    let Zip3 (a : int) (b : char) (c : bool) =
        Gen.zip3 (Gen.constant a) (Gen.constant b) (Gen.constant c)
        |> sample1
        |> ((=) (a, b, c))

    [<Property>]
    let Unzip (a : char) (b : int) =
        let f, g = Gen.constant (a, b) |> Gen.unzip
        (sample1 f, sample1 g) = (a, b)

    [<Property>]
    let Unzip3 (a : char) (b : int) (c : bool) =
        let f, g, h = Gen.constant (a, b, c) |> Gen.unzip3
        (sample1 f, sample1 g, sample1 h) = (a, b, c)

    [<Property>]
    let Two (v:int) =
        Gen.two (Gen.constant v)
        |> sample1
        |> ((=) (v,v))
     
    [<Property>]
    let Three (v:int) =
        Gen.three (Gen.constant v)
        |> sample1
        |> ((=) (v,v,v))
    
    [<Property>]
    let Four (v:int) =
        Gen.four (Gen.constant v)
        |> sample1
        |> ((=) (v,v,v,v))
    
    [<Property>]
    let Sequence (l:list<int>) =
        l |> List.map Gen.constant
        |> Gen.sequence
        |> sample1
        |> ((=) l)
     
    [<Property>]
    let ListOfLength (v:char) (PositiveInt length) =
        Gen.listOfLength length (Gen.constant v)
        |> sample1
        |> ((=) (List.init length (fun _ -> v)))

    [<Property>]
    let ``shuffle generates a permutation`` (xs : int array) =
        Gen.shuffle xs
        |> sample1
        |> Array.sort
        |> ((=) (Array.sort xs))

    // This property is non-deterministic, and may rarely fail. The chance of
    // this property not holding in case of a uniform shuffle - in other words,
    // the chance of producing 10 times the same input list of length 3 - is
    // (1/3!)^20, which should be small enough.
    // In this case, relying on the statistical attribute of a test being
    // exceedingly unlikely to produce a False Positive is sometimes the only
    // option. See also: https://github.com/fscheck/FsCheck/pull/221/
    [<Property>]
    let Shuffle (s:Set<int>) =
        s.Count > 2 ==> lazy
        let l = Set.toList s
        Gen.shuffle l
        |> sample 20
        |> List.map Seq.toList
        |> List.exists ((<>) l)

    [<Fact>]
    let ``Shuffle returns different array instances``() =
        Gen.shuffle [| 1..30 |]
        |> sample 20
        |> List.pairwise
        |> List.forall (fun (l,r) -> not <| obj.ReferenceEquals(l,r))

    [<Property>]
    let Piles (k:int) (sum:int) =
        let sample = Gen.piles k sum |> sample 10
        
        test <@ sample |> List.pairwise |> List.forall (fun (l,r) -> (l.Length = 0 && r.Length = 0) || not <| obj.ReferenceEquals(l,r)) @>
        test <@ sample
                |> List.forall (fun l -> 
                    l.Length = max 0 k // if k <= 0 empty list is returned 
                    && Array.sum l = (if k <= 0 then 0 else sum)
                    && if sum >= 0 then Array.forall (fun e -> e >= 0) l else true) @>
        //|> Prop.collect (k, sum, sample)
   
    [<Property>]
    let TryWhere (v:int) (predicate:int -> bool) =
        let expected = if predicate v then Some v else None
        Gen.tryWhere predicate (Gen.constant v)
        |> sample1
        |> ((=) expected)
    
    [<Property>]
    let Where (v:int) =
        Gen.where ((<=) 0) (Gen.elements [v;abs v])
        |> sample1
        |> ((=) (abs v))
    
    [<Property>]
    let ListOf (NonNegativeInt size) (v:char) =
        Gen.resize size (Gen.listOf <| Gen.constant v)
        |> sample 10
        |> List.forall (fun l -> l.Length <= size && List.forall ((=) v) l)


    [<Property>]
    let ``ListOf high dimension``(NonNegativeInt size) (v:int) =
        let l = Gen.resize size (Gen.listOf (Gen.listOf (Gen.listOf (Gen.listOf <| Gen.constant v))))
                |> sample 10
        l
        |> List.forall (fun l -> l.Length <= size && List.forall (List.forall (List.forall (List.forall ((=) v)))) l)
        |> Prop.collect l.Head

    
    [<Property>]
    let NonEmptyListOf (NonNegativeInt size) (v:string) =
        let actual = Gen.resize size (Gen.nonEmptyListOf <| Gen.constant v) |> sample 10
        actual
        |> List.forall (fun l -> 0 < l.Length && l.Length <= max 1 size && List.forall ((=) v) l)
    
    [<Property>]
    let SubListOf (l:list<int>) =
        Gen.subListOf l
        |> sample 10
        |> List.forall (fun sublist -> 
            List.length sublist <= List.length l
            && List.forall (fun e -> List.exists ((=) e) l) sublist)

    [<Property>]
    let ArrayOf (NonNegativeInt size) (v:int) =
        Gen.resize size (Gen.arrayOf <| Gen.constant v)
        |> sample 10
        |> List.forall (fun l -> l.Length <= size && Array.forall ((=) v) l)

    [<Property>]
    let ``ArrayOf high dimension`` (NonNegativeInt size) (v:int) =
        Gen.resize size (Gen.arrayOf (Gen.arrayOf (Gen.arrayOf (Gen.arrayOf <| Gen.constant v))))
        |> sample 10
        |> List.forall (fun l -> l.Length <= size && (Array.forall (Array.forall (Array.forall (Array.forall ((=) v)))) l))
    
    [<Property>]
    let ArrayOfLength (v:char) (PositiveInt length) =
        Gen.arrayOfLength length (Gen.constant v)
        |> sample1
        |> ((=) (Array.init length (fun _ -> v)))
    
    [<Property>]
    let Array2DOf (NonNegativeInt size) (v:int) =
        Gen.resize size (Gen.array2DOf (Gen.constant v))
        |> sample1
        |> fun arr ->
            (arr.Length <= size+1) 
            && (seq { for elem in arr do yield elem :?> int} |> Seq.forall ((=) v))
  
    [<Property>]
    let Array2DOfDim (NonNegativeInt rows,NonNegativeInt cols) (v:int) =
        (Gen.array2DOfDim (rows,cols) (Gen.constant v))
        |> sample1
        |> fun arr ->
            (Array2D.length1 arr <= rows) 
            && (Array2D.length2 arr <= cols) 
            && (seq { for elem in arr do yield elem :?> int} |> Seq.forall ((=) v))

    type MaybeNull =
        {
            SomeField : int
        }

    [<CLIMutable>]
    type MaybeHaveNull =
        {
             MightBeNull : MaybeNull
        }

    [<Fact>]
    let ``should support null as a shrunken value even without AllowNullLiteral (for C# compatibility)``() =
        let der = Arb.Default.Derive()
        let mhn = { MightBeNull = { SomeField = 5 } }
        let prop = mhn.GetType().GetProperty("MightBeNull", BindingFlags.Public ||| BindingFlags.Instance)
        prop.SetValue(mhn, null)
        // ensure that no exception occurs
        der.Shrinker mhn
          |> Seq.length
          |> ignore

    [<Fact>]
    let ``should generate functions from nullable values``() =
        let generated =
            Arb.generate<unit->int>
            |> sample1
            |> List.replicate 10
            |> List.map ((|>) ())
        generated
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> a = b)
        |> fun l -> test <@ Seq.forall id l @>

    type FunctorLaws<'a,'b,'c when 'a:equality and 'b :equality and 'c:equality> =
        static member Identity (x :'a) =
            let x' =  Gen.constant x
            let a = sample 10 (id x')
            let b = List.replicate 10 (id x)
            a = b

        static member Distribution (x:'a) (f:'b ->'c) (g:'a->'b) =
            let a = Gen.map (f << g) (Gen.constant x)
            let b = (Gen.map f << Gen.map g) (Gen.constant x)
            sample 10 a = sample 10 b

    [<Fact>]
    let ``should satisfy Functor laws``() =
        Check.QuickThrowOnFailureAll<FunctorLaws<_,_,_>>()

    // Thank you mausch: http://bugsquash.blogspot.com/2010/12/zipping-with-applicative-functors-in-f.html
    type ApplicativeLaws<'a,'b,'c when 'a:equality and 'b:equality>() =

        static member Identity (v: 'a) = 
            let a = Gen.constant v
            let x = Gen.constant id <*> a
            sample 10 x |> List.forall ((=) v)

        static member Composition (u: 'a -> 'b) (v: 'c -> 'a) (w: 'c) = 
            let u',v',w' = Gen.constant u, Gen.constant v, Gen.constant w
            let a = Gen.constant (<<) <*> u' <*> v' <*> w'
            let b = u' <*> (v' <*> w')
            sample 10 a = sample 10 b

        static member Homomorphism (f: 'a -> 'b) x = 
            let a = Gen.constant f <*> Gen.constant x
            let b = Gen.constant (f x)
            sample 10 a = sample 10 b

        static member Interchange (u: 'a -> 'b) (y: 'a) = 
            let u' = Gen.constant u
            let a = u' <*> Gen.constant y
            let b = Gen.constant ((|>) y) <*> u'
            sample 10 a = sample 10 b

    [<Fact>]
    let ``should satisfy Applicative Functor laws``() =
        Check.QuickThrowOnFailureAll<ApplicativeLaws<_,_,_>>()

    type MonadLaws<'a,'b,'c when 'a : equality and 'b : equality and 'c:equality>() =
        static member LeftIdentity (a:'a) (f:'a -> 'b) =
            let f = f >> Gen.constant
            let left = Gen.constant a >>= f 
            let right = f a
            sample 10 left = sample 10 right 
        static member RightIdentity (a:'a) =
            let m = Gen.constant a
            let left = m >>= Gen.constant
            let right = m
            sample 10 left = sample 10 right
        static member Associativity (a:'a) (f:'a->'b) (g:'b->'c) =
            let m = Gen.constant a
            let f = f >> Gen.constant
            let g = g >> Gen.constant
            let left = (m >>= f) >>= g
            let right =  m >>= (fun x -> f x >>= g)
            sample 10 left = sample 10 right

    [<Fact>]
    let ``should satisfy Monad laws``() =
        Check.QuickThrowOnFailureAll<MonadLaws<_,_,_>>()

    [<Fact>]
    let ``GenBuilder.While``() =
        // same as Gen.constant n, just to test while
        let convolutedGenNumber n =
            gen { let s = ref 0
                  while !s < n do
                    s := !s + 1
                  return !s
                }
        test <@ convolutedGenNumber 100 
                |> Gen.sample 1 10
                |> Seq.forall ((=) 100) @>

    [<Fact>]
    let ``GenBuilder.For``() =
        // same as Gen.constant n, just to test while
        let convolutedGenNumber n =
            gen { let s = ref 0
                  for _ in [1..n] do
                    s := !s + 1
                  return !s
                }
        test <@ convolutedGenNumber 100 
                |> Gen.sample 1 10
                |> Seq.forall ((=) 100) @>


    type MyRecord =
        { FieldA    : int
          FieldB    : MyUnion
          FieldC    : MyUnion }

    and MyUnion = 
    | NonRec of int
    | Rec    of MyRecord
    | Rec2   of MyOtherUnion

    and MyOtherUnion =
    | NonRec2 of int
    | Rec2 of MyUnion * MyRecord

    [<Fact>]
    let ``should generate recursive types``() =
        // this should just not stack overflow.
        // It used to, because the detection of whether a union case
        // is recursive or not, was limited to self-recursion (i.e. 
        // type Tree = T of Tree * Tree |  Branch of int
        // whereas as the example demonstrates recursion can be
        // more involved.
        let r1 = Gen.sample 100 100 (Arb.Default.Derive<MyRecord>().Generator)
        let r2 = Gen.sample 100 100 (Arb.Default.Derive<MyUnion>().Generator)
        let r3 = Gen.sample 100 100 (Arb.Default.Derive<MyOtherUnion>().Generator)
        test <@ r1.Length + r2.Length + r3.Length = 300 @>


    // these types were taking from the FSharpLu project, as there was some regression
    // when these types, or a subset of them, was generated.

    type WithFields = SomeField of int * int
    type SimpleDu = Foo | FooBar | Bar
    type ComplexDu = ComplexDu of WithFields | SimpleDU | AString of string
    type 'a RecursiveList = RecListLeaf of 'a | RecListCons of 'a RecursiveList
    type OptionOfBase = int option
    type OptionOfDu = SimpleDu option
    type Color = Red | Blue
    type Shape = Circle of int * int | Rectangle
    type 'a Tree = Leaf of 'a | Node of 'a Tree * 'a Tree
    type 'a Test = Case1 | Case2 of int | Case3 of int * string * 'a
    type MapType = Map<string,Color>
    type 'a NestedOptions = 'a option option option option

    type 'a Wrapper = { WrappedField : 'a }
    type NestedStructure = { subField : int }
    type NestedOptionStructure = { field : NestedStructure option }

    module SomeAmbiguity =
        type 'a RecordWithFieldNamedSome = { Some : 'a }
        type DUWithFieldlessCaseNamedSome = Some of string | Bla
        type DUWithCaseWithFieldNamedSome = Some | Bla
        type 'a Ambiguous1 = 'a RecordWithFieldNamedSome option
        type Ambiguous2 = DUWithFieldlessCaseNamedSome option
        type Ambiguous3 = DUWithCaseWithFieldNamedSome option


    [<Fact>]
    let ``generate FSharpLu test types``() =
        let size = 25
        let times = 10000
        let r = Gen.sample size times Arb.generate<ComplexDu>
        let r = Gen.sample size times Arb.generate<ComplexDu RecursiveList>
        let r = Gen.sample size times Arb.generate<WithFields>
        let r = Gen.sample size times Arb.generate<SimpleDu>
        let r = Gen.sample size times Arb.generate<ComplexDu>
        let r = Gen.sample size times Arb.generate<OptionOfBase>
        let r = Gen.sample size times Arb.generate<OptionOfDu>
        let r = Gen.sample size times Arb.generate<Color>
        let r = Gen.sample size times Arb.generate<Shape>
        let r = Gen.sample size times Arb.generate<int Tree>
        let r = Gen.sample size times Arb.generate<int Tree Test>
        let r = Gen.sample size times Arb.generate<int Test>
        let r = Gen.sample size times Arb.generate<int list Tree>
        let r = Gen.sample size times Arb.generate<string NestedOptions>
        let r = Gen.sample size times Arb.generate<string>
        let r = Gen.sample size times Arb.generate<string option>
        let r = Gen.sample size times Arb.generate<string option option>
        let r = Gen.sample size times Arb.generate<string option option option option>
        let r = Gen.sample size times Arb.generate<int NestedOptions>
        let r = Gen.sample size times Arb.generate<SomeAmbiguity.Ambiguous1<string>>
        let r = Gen.sample size times Arb.generate<SomeAmbiguity.Ambiguous1<SimpleDu>>
        let r = Gen.sample size times Arb.generate<NestedOptionStructure>
        let r = Gen.sample size times Arb.generate<SomeAmbiguity.Ambiguous2>
        let r = Gen.sample size times Arb.generate<SomeAmbiguity.Ambiguous3>
        ()

    [<Fact>]
    let ``generating large array should work``() =
        // occassionally there have been bugs where large array generations causes stack overflow
        Gen.choose(0,100) |> Gen.arrayOf |> Gen.sample 30000 100

    [<Fact>]
    let ``sublistof for an array with single element should generate not only empty list``() =
        let result = 
            Gen.subListOf [1] 
            |> Gen.sample 100 100
            |> Seq.contains [1]
        test <@ result @>

    [<Property>]
    let ValueTuple2 (a : int) (b : char) =
        Gen.valueTuple2 (Gen.constant a) (Gen.constant b)
        |> sample1
        |> ((=) struct (a, b))


    [<Property>]
    let ValueTuple3 (a : int) (b : char) (c : bool) =
        Gen.valueTuple3 (Gen.constant a) (Gen.constant b) (Gen.constant c)
        |> sample1
        |> ((=) struct (a, b, c))

    [<Property>]
    let ValueTuple4 (a : int) (b : char) (c : bool) (d : int) =
        Gen.valueTuple4 (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d)
        |> sample1
        |> ((=) struct (a, b, c, d))

    [<Property>]
    let ValueTuple5 (a : int) (b : char) (c : bool) (d : int) (e : char) =
        Gen.valueTuple5 (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d) (Gen.constant e)
        |> sample1
        |> ((=) struct (a, b, c, d, e))

    [<Property>]
    let ValueTuple6 (a : int) (b : char) (c : bool) (d : int) (e : char) (f : bool) =
        Gen.valueTuple6 (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d) (Gen.constant e) (Gen.constant f)
        |> sample1
        |> ((=) struct (a, b, c, d, e, f))
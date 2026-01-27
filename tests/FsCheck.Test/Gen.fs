namespace FsCheck.Test

module Gen =

    open Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open Helpers
    open Swensen.Unquote
    open System.Reflection

    [<Property>]
    let Choose (Interval (l,h)) = 
        assertTrue ( Gen.choose (l,h)
                |> sample 10
                |> Seq.forall (fun v -> l <= v && v <= h) )
    
    [<Property>] 
    let Elements (l:list<char>) =
        not l.IsEmpty ==> 
        lazy (  assertTrue ( Gen.elements l
                        |> sample 50
                        |> Seq.forall (isIn l) ) )

    [<Fact>]
    let GrowingElements () =
        let sizes = [ 1..100 ]
        let run g =
            [ for s in sizes -> Gen.resize s g ]
            |> Gen.sequenceToList
            |> sample1

        let actual = Gen.growingElements sizes |> run

        assertTrue ( List.forall2 (>=) sizes actual )

    [<Property>]
    let constant (v : (int * char)) =
        assertTrue ( Gen.constant v
                |> sample 10
                |> Array.forall (fun actual -> obj.ReferenceEquals(actual, v)) )

    [<Property>]
    let fresh ((i:int,c:char) as v) =
        assertTrue ( Gen.fresh (fun () -> (i,c)) 
                |> sample 10
                |> Seq.forall (fun actual -> not (obj.ReferenceEquals(actual, v)) && actual = v) )
    
    [<Property>]
    let Oneof (l:list<string>) =
        not l.IsEmpty ==> 
        lazy (  assertTrue ( List.map Gen.constant l
                        |> Gen.oneof
                        |> sample 10
                        |> Seq.forall (isIn l) ))

    [<Property>]
    let Frequency (frequencies:list<PositiveInt*string>) =
        let generatedValues = frequencies |> List.filter (fst >> (fun p -> p.Get) >> (<>) 0) |> List.map snd
        not generatedValues.IsEmpty ==>
         lazy ( assertTrue ( frequencies
                        |> List.map (fun (freq,s) -> (freq.Get,Gen.constant s))
                        |> Gen.frequency
                        |> sample 100
                        |> Seq.forall (isIn generatedValues) ))

    [<Fact>]
    let ``frequency should throw argument exception if no element can be generated``() =
        let prop = lazy (Gen.frequency [(0,Gen.constant 1)]) //used lazy since unquote couldn't handle generator directly
        raises<System.ArgumentException> <@ prop.Value @>
        
    
    [<Property>]
    let Map (f:string -> int) v =
        assertTrue ( Gen.map f (Gen.constant v)
                |> sample 1
                |> Seq.forall ((=) (f v)) )
        
    [<Property>]
    let Map2 (f:char -> int -> int) a b =
        assertTrue ( Gen.map2 f (Gen.constant a) (Gen.constant b)
                |> sample1
                |> ((=) (f a b)) )
        
    [<Property>]
    let Map3 (f:int -> char -> int -> int) a b c =
        assertTrue ( Gen.map3 f (Gen.constant a) (Gen.constant b) (Gen.constant c)
                |> sample1
                |> ((=) (f a b c)) )
        
    [<Property>]
    let Map4 (f:char -> int -> char -> bool -> int) a b c d =
        assertTrue ( Gen.map4 f (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d)
                |> sample1
                |> ((=) (f a b c d)) )
       
    [<Property>]
    let Map5 (f:bool -> char -> int -> char -> bool -> int) a b c d e =
        assertTrue ( Gen.map5 f (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d) (Gen.constant e)
                |> sample1
                |> ((=) (f a b c d e)) )
    
    [<Property>]
    let Map6 (f:bool -> char -> int -> char -> bool -> int -> char ) a b c d e g = 
        assertTrue ( Gen.map6 f (Gen.constant a) (Gen.constant b) (Gen.constant c) (Gen.constant d) (Gen.constant e) (Gen.constant g)
                |> sample1
                |> ((=) (f a b c d e g)) )

    [<Property>]
    let Zip (a : int) (b : char) =
        assertTrue ( Gen.zip (Gen.constant a) (Gen.constant b)
                |> sample1
                |> ((=) (a, b)) )

    [<Property>]
    let Zip3 (a : int) (b : char) (c : bool) =
        assertTrue ( Gen.zip3 (Gen.constant a) (Gen.constant b) (Gen.constant c)
                |> sample1
                |> ((=) (a, b, c)) )

    [<Property>]
    let Two (v:int) =
        assertTrue ( Gen.two (Gen.constant v)
                |> sample1
                |> ((=) (v,v)) )
     
    [<Property>]
    let Three (v:int) =
        assertTrue ( Gen.three (Gen.constant v)
                |> sample1
                |> ((=) (v,v,v)) )
    
    [<Property>]
    let Four (v:int) =
        assertTrue ( Gen.four (Gen.constant v)
                |> sample1
                |> ((=) (v,v,v,v)) )
    
    [<Property>]
    let Collect (l:list<int>) =
        Gen.collectToList Gen.constant l
        |> sample1
        |> ((=) l)

    [<Property>]
    let CollectToSeq (l:list<byte>) =
        Seq.ofList l
        |> Gen.collectToSeq Gen.constant
        |> sample1
        |> List.ofSeq
        |> ((=) l)

    [<Property>]
    let CollectToArr (a:array<char>) =
        Gen.collectToArray Gen.constant a
        |> sample1
        |> ((=) a)

    [<Property>]
    let Sequence (l:list<int>) =
        l |> List.map Gen.constant
        |> Gen.sequenceToList
        |> sample1
        |> ((=) l)

    [<Property>]
    let SequenceToSeq (l:list<byte>) =
        Seq.ofList l
        |> Seq.map Gen.constant
        |> Gen.sequenceToSeq
        |> sample1
        |> List.ofSeq
        |> ((=) l)
     
    [<Property>]
    let SequenceToArr (a:array<char>) =
        Array.map Gen.constant a
        |> Gen.sequenceToArray
        |> sample1
        |> ((=) a)

    [<Property>]
    let ListOfLength (v:char) (PositiveInt length) =
        assertTrue ( Gen.listOfLength length (Gen.constant v)
                |> sample1
                |> ((=) (List.init length (fun _ -> v))) )

    [<Property>]
    let ``shuffle generates a permutation`` (xs : int array) =
        assertTrue ( Gen.shuffle xs
                |> sample1
                |> Array.sort
                |> ((=) (Array.sort xs)) )

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
        assertTrue ( Gen.shuffle l
                |> sample 20
                |> Seq.map Seq.toList
                |> Seq.exists ((<>) l) )

    [<Fact>]
    let ``Shuffle returns different array instances``() =
        Gen.shuffle [| 1..30 |]
        |> sample 20
        |> Array.pairwise
        |> Array.iter (fun (l,r) -> assertTrue ( not <| obj.ReferenceEquals(l,r) ))

    [<Property>]
    let Piles (k:int) (sum:int) =
        let sample = Gen.piles k sum |> sample 10
        
        assertTrue ( sample |> Array.pairwise |> Array.forall (fun (l,r) -> (l.Length = 0 && r.Length = 0) || not <| obj.ReferenceEquals(l,r)) )
        assertTrue ( sample
                |> Array.forall (fun l -> 
                    l.Length = max 0 k // if k <= 0 empty list is returned 
                    && Array.sum l = (if k <= 0 then 0 else sum)
                    && if sum >= 0 then Array.forall (fun e -> e >= 0) l else true) )
        //|> Prop.collect (k, sum, sample)
   
    [<Property>]
    let TryWhere (v:int) (predicate:int -> bool) =
        let expected = if predicate v then Some v else None
        assertTrue ( Gen.tryWhere predicate (Gen.constant v)
                |> sample1
                |> ((=) expected) )
    
    [<Property>]
    let Where (v:int) =
        assertTrue ( Gen.where ((<=) 0) (Gen.elements [v;abs v])
                |> sample1
                |> ((=) (abs v)) )
    
    [<Property>]
    let TryPick (v:int) (chooser:int -> option<string>) =
        let expected = chooser v
        assertTrue ( Gen.tryPick chooser (Gen.constant v)
                |> sample1
                |> ((=) expected) )
    
    [<Property>]
    let Pick (v:int) =
        let chooser x = if x >= 0 then Some (string x) else None
        assertTrue ( Gen.pick chooser (Gen.elements [v;abs v])
                |> sample1
                |> ((=) (string (abs v))) )
    
    [<Property>]
    let ListOf (NonNegativeInt size) (v:char) =
        assertTrue ( Gen.resize size (Gen.listOf <| Gen.constant v)
                |> sample 10
                |> Seq.forall (fun l -> l.Length <= size && Seq.forall ((=) v) l) )


    [<Property>]
    let ``ListOf high dimension``(NonNegativeInt size) (v:int) =
        let l = Gen.resize size (Gen.listOf (Gen.listOf (Gen.listOf (Gen.listOf <| Gen.constant v))))
                |> sample 10
        assertTrue ( l |> Seq.forall (fun l -> l.Length <= size && Seq.forall (Seq.forall (Seq.forall (Seq.forall ((=) v)))) l) )

    
    [<Property>]
    let NonEmptyListOf (NonNegativeInt size) (v:string) =
        let actual = Gen.resize size (Gen.nonEmptyListOf <| Gen.constant v) |> sample 10
        assertTrue ( actual |> Seq.forall (fun l -> 0 < l.Length && l.Length <= max 1 size && Seq.forall ((=) v) l) )
    
    [<Property>]
    let SubListOf (l:list<int>) =
        Gen.subListOf l
        |> sample 10
        |> Seq.iter (fun sublist -> 
            assertTrue (List.length sublist <= List.length l)
            assertTrue (Seq.forall (fun e -> List.exists ((=) e) l) sublist))

    [<Property>]
    let ArrayOf (NonNegativeInt size) (v:int) =
        Gen.resize size (Gen.arrayOf <| Gen.constant v)
        |> sample 10
        |> Seq.iter (fun l -> assertTrue (l.Length <= size && Array.forall ((=) v) l))

    [<Property>]
    let ``ArrayOf high dimension`` (NonNegativeInt size) (v:int) =
        Gen.resize size (Gen.arrayOf (Gen.arrayOf (Gen.arrayOf (Gen.arrayOf <| Gen.constant v))))
        |> sample 10
        |> Seq.iter (fun l -> 
            assertTrue (l.Length <= size )
            assertTrue (Array.forall (Array.forall (Array.forall (Array.forall ((=) v)))) l))
    
    [<Property>]
    let ArrayOfLength (v:char) (PositiveInt length) =
        let arr = Gen.arrayOfLength length (Gen.constant v) |> sample1
        assertTrue (arr = (Array.init length (fun _ -> v)))
    
    [<Property>]
    let Array2DOf (NonNegativeInt size) (v:int) =
        let arr = Gen.resize size (Gen.array2DOf (Gen.constant v)) |> sample1
        assertTrue (arr.Length <= size+1) 
        assertTrue (seq { for elem in arr do yield elem :?> int} |> Seq.forall ((=) v))
  
    [<Property>]
    let Array2DOfDim (NonNegativeInt rows,NonNegativeInt cols) (v:int) =
        let arr = (Gen.array2DOfDim rows cols (Gen.constant v)) |> sample1
        assertTrue (Array2D.length1 arr <= rows)
        assertTrue (Array2D.length2 arr <= cols) 
        assertTrue (seq { for elem in arr do yield elem :?> int} |> Seq.forall ((=) v))
    
    [<Property>]
    let ``ScaleSize works`` (PositiveInt size) =
        ArbMap.defaults
        |> ArbMap.generate<int>
        |> Gen.scaleSize (fun n -> n / 10) 
        |> Gen.sampleWithSize size 100 
        |> Array.forall (fun i -> i <= size / 10)
        |> assertTrue

    type MaybeNull = { SomeField : int }

    [<CLIMutable>]
    type MaybeHaveNull = { MightBeNull : MaybeNull }

    [<Fact>]
    let ``should support null as a shrunk value even without AllowNullLiteral (for C# compatibility)``() =
        let der = ArbMap.defaults |> ArbMap.arbitrary
        let mhn = { MightBeNull = { SomeField = 5 } }
        let prop = mhn.GetType().GetProperty("MightBeNull", BindingFlags.Public ||| BindingFlags.Instance)
        prop.SetValue(mhn, null)
        // ensure that no exception occurs
        der.Shrinker mhn
        |> Seq.length
        |> ignore

    [<Fact>]
    let ``should generate functions from null``() =
        let generated =
            // fsharplint:disable-next-line Hints
            ArbMap.defaults
            |> ArbMap.generate<unit->int>
            |> sample1
            |> List.replicate 10
            |> List.map ((|>) ())
        generated
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> a = b)
        |> fun l -> assertTrue ( Seq.forall id l )

    [<Fact>]
    let ``should generate pure functions``() =
         let generated =
             // fsharplint:disable-next-line Hints
             ArbMap.defaults
             |> ArbMap.generate<int->int>
             |> sample1
             |> List.replicate 10
             |> List.map ((|>) 2)
         generated
         |> Seq.pairwise
         |> Seq.map (fun (a,b) -> a = b)
         |> fun l -> assertTrue ( Seq.forall id l )

    type FunctorLaws<'a,'b,'c when 'a:equality and 'b :equality and 'c:equality> =
        static member Identity (x :'a) =
            let x' =  Gen.constant x
            let a = sample 10 (id x') // fsharplint:disable-line Hints
            let b = Array.replicate 10 (id x) // fsharplint:disable-line Hints
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
            sample 10 x |> Seq.forall ((=) v)

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
    let ``GenBuilder While``() =
        // same as Gen.constant n, just to test while
        let convolutedGenNumber n =
            gen { let s = ref 0
                  while !s < n do
                    s := !s + 1
                  return !s
                }
        assertTrue ( convolutedGenNumber 100 
                |> Gen.sample 10
                |> Seq.forall ((=) 100) )

    [<Fact>]
    let ``GenBuilder For``() =
        // same as Gen.constant n, just to test while
        let convolutedGenNumber n =
            gen { let s = ref 0
                  for _ in [1..n] do
                    s := !s + 1
                  return !s
                }
        assertTrue ( convolutedGenNumber 100 
                |> Gen.sample 10
                |> Seq.forall ((=) 100) )


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
        let r1 = Gen.sampleWithSize 100 100 (ArbMap.defaults.ArbFor<MyRecord>().Generator)
        let r2 = Gen.sampleWithSize 100 100 (ArbMap.defaults.ArbFor<MyUnion>().Generator)
        let r3 = Gen.sampleWithSize 100 100 (ArbMap.defaults.ArbFor<MyOtherUnion>().Generator)
        assertTrue ( r1.Length + r2.Length + r3.Length = 300 )


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
    type NestedStructure = { SubField : int }
    type NestedOptionStructure = { Field : NestedStructure option }

    module SomeAmbiguity =
        type 'a RecordWithFieldNamedSome = { Some : 'a }
        type DUWithFieldlessCaseNamedSome = Some of string | Bla
        type DUWithCaseWithFieldNamedSome = Some | Bla
        type 'a Ambiguous1 = 'a RecordWithFieldNamedSome option
        type Ambiguous2 = DUWithFieldlessCaseNamedSome option
        type Ambiguous3 = DUWithCaseWithFieldNamedSome option


    [<Fact>]
    let ``generate FSharpLu test types``() =
        let times = 10000
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<ComplexDu>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<ComplexDu RecursiveList>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<WithFields>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<SimpleDu>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<ComplexDu>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<OptionOfBase>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<OptionOfDu>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<Color>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<Shape>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<int Tree>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<int Tree Test>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<int Test>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<int list Tree>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<string NestedOptions>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<string>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<string option>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<string option option>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<string option option option option>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<int NestedOptions>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<SomeAmbiguity.Ambiguous1<string>>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<SomeAmbiguity.Ambiguous1<SimpleDu>>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<NestedOptionStructure>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<SomeAmbiguity.Ambiguous2>)
        let r = Gen.sample times (ArbMap.defaults |> ArbMap.generate<SomeAmbiguity.Ambiguous3>)
        ()

    [<Fact>]
    let ``generating large array should work``() =
        // occasionally there have been bugs where large array generations causes stack overflow
        Gen.choose(0,100) |> Gen.arrayOf |> Gen.sampleWithSize 30000 100

    [<Fact>]
    let ``sublistof for an array with single element should generate not only empty list``() =
        let result = 
            Gen.subListOf [1] 
            |> Gen.sampleWithSize 100 100
            |> Seq.contains [1]
        test <@ result @>  

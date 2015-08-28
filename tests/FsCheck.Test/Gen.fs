namespace FsCheck.Test

module Gen =

    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Helpers

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
    let SuchThatOption (v:int) (predicate:int -> bool) =
        let expected = if predicate v then Some v else None
        Gen.suchThatOption predicate (Gen.constant v)
        |> sample1
        |> ((=) expected)
//        |> classify expected.IsNone "None"
//        |> classify expected.IsSome "Some"
    
    [<Property>]   
    let SuchThat (v:int) =
        Gen.suchThat ((<=) 0) (Gen.elements [v;abs v])
        |> sample1
        |> ((=) (abs v))
    
    [<Property>]
    let ListOf (NonNegativeInt size) (v:char) =
        Gen.resize size (Gen.listOf <| Gen.constant v)
        |> sample 10
        |> List.forall (fun l -> l.Length <= size+1 && List.forall ((=) v) l)
    
    [<Property>]
    let NonEmptyListOf (NonNegativeInt size) (v:string) =
        let actual = Gen.resize size (Gen.nonEmptyListOf <| Gen.constant v) |> sample 10
        actual
        |> List.forall (fun l -> 0 < l.Length && l.Length <= max 1 size && List.forall ((=) v) l) 
        //|> label (sprintf "Actual: %A" actual)
    
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
        |> List.forall (fun l -> l.Length <= size+1 && Array.forall ((=) v) l)
    
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

    [<Fact>]
    let ``should generate functions from nullable values``() =
        Runner.init.Force() |> ignore //in case this is run as the only test
        let generated =
            Arb.generate<unit->int>
            |> sample1
            |> List.replicate 10
            |> List.map ((|>) ())
        generated
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> a = b)
        |> fun l -> Assert.DoesNotContain(false, l)

    //variant generators should be independent...this is not a good check for that.
//    let Variant (v:char) =
//        Gen.variant v (Gen.constant v) |> sample1 |>  ((=) v)

    type FunctorLaws<'a,'b,'c when 'a:equality and 'b :equality and 'c:equality> =
        static member identity (x :'a) =
            let x' =  Gen.constant x
            let a = sample 10 (id x')
            let b = List.replicate 10 (id x)
            a = b

        static member distribution (x:'a) (f:'b ->'c) (g:'a->'b) =
            let a = Gen.map (f << g) (Gen.constant x)
            let b = (Gen.map f << Gen.map g) (Gen.constant x)
            sample 10 a = sample 10 b

    [<Fact>]
    let ``should satisfy Functor laws``() =
        Check.QuickThrowOnFailureAll<FunctorLaws<_,_,_>>()

    // Thank you mausch: http://bugsquash.blogspot.com/2010/12/zipping-with-applicative-functors-in-f.html
    type ApplicativeLaws<'a,'b,'c when 'a:equality and 'b:equality>() =

        static member identity (v: 'a) = 
            let a = Gen.constant v
            let x = Gen.constant id <*> a
            sample 10 x |> List.forall ((=) v)

        static member composition (u: 'a -> 'b) (v: 'c -> 'a) (w: 'c) = 
            let u',v',w' = Gen.constant u, Gen.constant v, Gen.constant w
            let a = Gen.constant (<<) <*> u' <*> v' <*> w'
            let b = u' <*> (v' <*> w')
            sample 10 a = sample 10 b

        static member homomorphism (f: 'a -> 'b) x = 
            let a = Gen.constant f <*> Gen.constant x
            let b = Gen.constant (f x)
            sample 10 a = sample 10 b

        static member interchange (u: 'a -> 'b) (y: 'a) = 
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
        convolutedGenNumber 100 
        |> Gen.sample 1 10
        |> Seq.forall ((=) 100) 
        |> Assert.True

    [<Fact>]
    let ``GenBuilder.For``() =
        // same as Gen.constant n, just to test while
        let convolutedGenNumber n =
            gen { let s = ref 0
                  for c in [1..100] do
                    s := !s + 1
                  return !s
                }
        convolutedGenNumber 100 
        |> Gen.sample 1 10
        |> Seq.forall ((=) 100) 
        |> Assert.True
#light

namespace FsCheck.Checks

module Common = 

    open FsCheck.Common

    let Memoize (f:int->string) (a:int) = memoize f a = f a

    let Flip (f: char -> int -> string) a b = flip f a b = f b a

module Generator =
    
    open FsCheck
    open FsCheck.Generator
    
    let sample n gn  = 
        let rec sample i seed samples =
            if i = 0 then samples
            else sample (i-1) (Random.stdSplit seed |> snd) (generate 1000 seed gn :: samples)
        sample n (Random.newSeed()) []
    
    let sample1 gn = sample 1 gn |> List.hd
    
    type Interval = Interval of int * int
    type NonNegativeInt = NonNegative of int
    type NonZeroInt = NonZero of int
    type PositiveInt = Positive of int
    let unPositive (Positive i) = i
    
    type Arbitraries =
        //generates an interval between two positive ints
        static member Interval() =
            { new Arbitrary<Interval>() with
                override  x.Arbitrary = 
                    gen { 
                        let! start,offset = two arbitrary
                        return Interval (abs start,abs start+abs offset)
                    }
             }
        static member NonNegativeInt() =
            { new Arbitrary<NonNegativeInt>() with
                override x.Arbitrary = arbitrary |> fmapGen (NonNegative << abs)
                override x.CoArbitrary (NonNegative i) = coarbitrary i
                override x.Shrink (NonNegative i) = shrink i |> Seq.filter ((<) 0) |> Seq.map NonNegative }
        static member NonZeroInt() =
            { new Arbitrary<NonZeroInt>() with
                override x.Arbitrary = arbitrary |> suchThat ((<>) 0) |> fmapGen NonZero 
                override x.CoArbitrary (NonZero i) = coarbitrary i
                override x.Shrink (NonZero i) = shrink i |> Seq.filter ((=) 0) |> Seq.map NonZero }
        static member PositiveInt() =
            { new Arbitrary<PositiveInt>() with
                override x.Arbitrary = arbitrary |> suchThat ((<>) 0) |> fmapGen (Positive << abs) 
                override x.CoArbitrary (Positive i) = coarbitrary i
                override x.Shrink (Positive i) = shrink i |> Seq.filter ((<=) 0) |> Seq.map Positive }
    registerGenerators<Arbitraries>()
    
    let Choose (Interval (l,h)) = 
        choose (l,h)
        |> sample 10
        |> List.for_all (fun v -> l <= v && v <= h)
     
    let private isIn l elem = List.mem elem l
       
    let Elements (l:list<char>) =
        not l.IsEmpty ==> 
        lazy (  elements l
                |> sample 50
                |> List.for_all (isIn l))
    
    let Constant (v : char) =
        constant v
        |> sample 10
        |> List.for_all ((=) v)
    
    let Oneof (l:list<string>) =
        not l.IsEmpty ==> 
        lazy (  List.map constant l
                |> oneof
                |> sample 50
                |> List.for_all (isIn l))
    
    let Frequency (l:list<NonNegativeInt*string>) =
        let generatedValues = l |> List.filter (fst >> (fun (NonNegative p) -> p) >> (<>) 0) |> List.map snd
        (sprintf "%A" generatedValues) @|
        (not generatedValues.IsEmpty ==>
         lazy ( List.map (fun (NonNegative freq,s) -> (freq,constant s)) l
                |> frequency
                |> sample 100
                |> List.for_all (isIn generatedValues)))
    
    let LiftGen (f:string -> int) v =
        liftGen f (constant v)
        |> sample 1
        |> List.for_all ((=) (f v))
        
    let LiftGen2 (f:char -> int -> int) a b =
        liftGen2 f (constant a) (constant b)
        |> sample1
        |> ((=) (f a b))
        
    let LiftGen3 (f:int -> char -> int -> int) a b c =
        liftGen3 f (constant a) (constant b) (constant c)
        |> sample1
        |> ((=) (f a b c))
        
    let LiftGen4 (f:char -> int -> char -> bool -> int) a b c d =
        liftGen4 f (constant a) (constant b) (constant c) (constant d)
        |> sample1
        |> ((=) (f a b c d))
        
    let LiftGen5 (f:bool -> char -> int -> char -> bool -> int) a b c d e =
        liftGen5 f (constant a) (constant b) (constant c) (constant d) (constant e)
        |> sample1
        |> ((=) (f a b c d e))
        
    let LiftGen6 (f:bool -> char -> int -> char -> bool -> int -> char ) a b c d e g =
        liftGen6 f (constant a) (constant b) (constant c) (constant d) (constant e) (constant g)
        |> sample1
        |> ((=) (f a b c d e g))
    
    let Two (v:int) =
        two (constant v)
        |> sample1
        |> ((=) (v,v))
        
    let Three (v:int) =
        three (constant v)
        |> sample1
        |> ((=) (v,v,v))
        
    let Four (v:int) =
        four (constant v)
        |> sample1
        |> ((=) (v,v,v,v))
        
    let Sequence (l:list<int>) =
        l |> List.map constant
        |> sequence
        |> sample1
        |> ((=) l)
        
    let VectorOf (v:char) (Positive length) =
        vectorOf length (constant v)
        |> sample1
        |> ((=) (List.init length (fun _ -> v)))
    
    let SuchThatOption (v:int) (predicate:int -> bool) =
        let expected = if predicate v then Some v else None
        suchThatOption predicate (constant v)
        |> sample1
        |> ((=) expected)
        |> classify expected.IsNone "None"
        |> classify expected.IsSome "Some"
        
    let SuchThat (v:int) =
        suchThat ((<=) 0) (elements [v;abs v])
        |> sample1
        |> ((=) (abs v))
    
    let ListOf (NonNegative size) (v:char) =
        resize size (listOf <| constant v)
        |> sample 10
        |> List.for_all (fun l -> l.Length <= size && List.for_all ((=) v) l)
    
    let NonEmptyListOf (NonNegative size) (v:string) =
        let actual = resize size (nonEmptyListOf <| constant v) |> sample 10
        actual
        |> List.for_all (fun l -> 0 < l.Length && l.Length <= max 1 size && List.for_all ((=) v) l) 
        |> label (sprintf "Actual: %A" actual)
    
    //variant generators should be independent...this is not a good check for that.
    let Variant (NonNegative var) (v:char) =
        variant var (constant v) |> sample1 |>  ((=) v)
    
    
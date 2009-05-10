(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2009 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck.Checks

open FsCheck

module Helpers = 

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
    type IntWithMax = IntWithMax of int
    
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
        static member IntWithMax() =
            { new Arbitrary<IntWithMax>() with
                override x.Arbitrary = frequency    [ (1,elements [IntWithMax Int32.max_int; IntWithMax Int32.min_int])
                                                    ; (10,arbitrary |> fmapGen IntWithMax) ] 
                override x.CoArbitrary (IntWithMax i) = coarbitrary i
                override x.Shrink (IntWithMax i) = shrink i |> Seq.map IntWithMax }
    do registerGenerators<Arbitraries>()

open Helpers

module Common = 

    open FsCheck.Common

    let Memoize (f:int->string) (a:int) = memoize f a = f a

    let Flip (f: char -> int -> string) a b = flip f a b = f b a
    
module Random =

    open FsCheck.Random

    let DivMod (x:int) (y:int) = 
        y <> 0 ==> lazy (let (d,m) = divMod x y in d*y + m = x)
        
    let MkStdGen (IntWithMax seed) =
        within 1000 <| lazy (let (StdGen (s1,s2)) = mkStdGen seed in s1 > 0 && s2 > 0 (*todo:add check*) ) //check for bug: hangs when seed = min_int
        //|> collect seed

module Generator = 

    open FsCheck.Generator
    
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
    
 
 module Functions =
 
    open FsCheck.Functions
    
    let Function (f:int->char) (vs:list<int>) =
        let tabledF = toFunction f
        (List.map tabledF.Value vs) = (List.map f vs)
        && List.for_all (fun v -> List.try_assoc v tabledF.Table = Some (f v)) vs
        
module Arbitrary =
    
    open FsCheck.Arbitrary
    open System
    
    let private addLabels (generator,shrinker) = ( generator |@ "Generator", shrinker |@ "Shrinker")
    
    let Unit() = 
        (   arbitrary<unit> |> sample 10 |> List.for_all ((=) ())
        ,   shrink<unit>() |> Seq.is_empty)
        |> addLabels
    
    let Boolean (b:bool) =
        (   arbitrary<bool> |> sample 10 |> List.for_all (fun v -> v  || true)
        ,    shrink<bool> b |> Seq.is_empty)
        |> addLabels
    
    let Int32 (NonNegative size) (v:int) =
        (   arbitrary<int> |> resize size |> sample 10 |> List.for_all (fun v -> -size <= v && v <= size)
        ,   shrink<int> v |> Seq.for_all (fun shrunkv -> shrunkv <= abs v))
            
    let Double (NonNegative size) (value:float) =
        (   arbitrary<float> |> resize size |> sample 10
            |> List.for_all (fun v -> 
                (-2.0 * float size <= v && v <= 2.0 * float size )
                || Double.IsNaN(v) || Double.IsInfinity(v)
                || v = Double.Epsilon || v = Double.MaxValue || v = Double.MinValue)
        ,   shrink<float> value 
            |> Seq.for_all (fun shrunkv -> shrunkv = 0.0 || shrunkv <= abs value))
        |> addLabels
    //String.
        
module Property =
    open FsCheck.Property
    open FsCheck.Generator
    open FsCheck.Common
    open System
    
    type SymProp =  | Unit | Bool of bool | Exception
                    | ForAll of int * SymProp
                    | Implies of bool * SymProp
                    | Classify of bool * string * SymProp
                    | Collect of int * SymProp
                    | Label of string * SymProp
                    | And of SymProp * SymProp
                    | Or of SymProp * SymProp
                    | Lazy of SymProp
                    | Tuple2 of SymProp * SymProp
                    | Tuple3 of SymProp * SymProp * SymProp //and 4,5,6
                    | List of SymProp list
     
    let rec private symPropGen =
        let rec recGen size =
            match size with
            | 0 -> oneof [constant Unit; liftGen (Bool) arbitrary; constant Exception]
            | n when n>0 ->
                let subProp = recGen (size/2)
                oneof   [ liftGen2 (curry ForAll) arbitrary (subProp)
                        ; liftGen2 (curry Implies) arbitrary (subProp)
                        ; liftGen2 (curry Collect) arbitrary (subProp)
                        ; liftGen3 (curry2 Classify) arbitrary arbitrary (subProp)
                        ; liftGen2 (curry Label) arbitrary (subProp)
                        ; liftGen2 (curry And) (subProp) (subProp)
                        ; liftGen2 (curry Or) (subProp) (subProp)
                        ; liftGen Lazy subProp
                        ; liftGen2 (curry Tuple2) subProp subProp
                        ; liftGen3 (curry2 Tuple3) subProp subProp subProp
                        ; liftGen List (resize 3 <| nonEmptyListOf subProp)
                        ]
            | _ -> failwith "symPropGen: size must be positive"
        sized recGen
                  
    let rec private determineResult prop =
        let addStamp stamp res = { res with Stamp = stamp :: res.Stamp }
        let addArgument arg res = { res with Arguments = arg :: res.Arguments }
        let addLabel label (res:Result) = { res with Labels = Set.add label res.Labels }
        let andCombine prop1 prop2 = let (r1:Result,r2) = determineResult prop1, determineResult prop2 in r1.And(r2)
        match prop with
        | Unit -> succeeded
        | Bool true -> succeeded
        | Bool false -> failed
        | Exception  -> exc <| InvalidOperationException()
        | ForAll (i,prop) -> determineResult prop |> addArgument i
        | Implies (true,prop) -> determineResult prop
        | Implies (false,_) -> rejected
        | Classify (true,stamp,prop) -> determineResult prop |> addStamp stamp
        | Classify (false,_,prop) -> determineResult prop
        | Collect (i,prop) -> determineResult prop |> addStamp (any_to_string i)
        | Label (l,prop) -> determineResult prop |> addLabel l
        | And (prop1, prop2) -> andCombine prop1 prop2
        | Or (prop1, prop2) -> let r1,r2 = determineResult prop1, determineResult prop2 in r1.Or(r2)
        | Lazy prop -> determineResult prop
        | Tuple2 (prop1,prop2) -> andCombine prop1 prop2
        | Tuple3 (prop1,prop2,prop3) -> (andCombine prop1 prop2).And(determineResult prop3)
        | List props -> List.fold_left (fun st p -> st.And(determineResult p)) (List.hd props |> determineResult) (List.tl props)
        
    let rec private toProperty prop =
        match prop with
        | Unit -> property ()
        | Bool b -> property b
        | Exception -> let p = (lazy (raise <| InvalidOperationException();())) in property p
        | ForAll (i,prop) -> forAll (constant i) (fun i -> toProperty prop)
        | Implies (b,prop) -> b ==> (toProperty prop)
        | Classify (b,stamp,prop) -> classify b stamp (toProperty prop)
        | Collect (i,prop) -> collect i (toProperty prop)
        | Label (l,prop) -> label l (toProperty prop)
        | And (prop1,prop2) -> (toProperty prop1) .&. (toProperty prop2)
        | Or (prop1,prop2) -> (toProperty prop1) .|. (toProperty prop2)
        | Lazy prop -> toProperty prop
        | Tuple2 (prop1,prop2) -> (toProperty prop1) .&. (toProperty prop2)
        | Tuple3 (prop1,prop2,prop3) -> (toProperty prop1) .&. (toProperty prop2) .&. (toProperty prop3)
        | List props -> List.fold_left (fun st p -> st .&. toProperty p) (List.hd props |> toProperty) (List.tl props)
    
    let private areSame (r0:Result) (r1:Result) =
        match r0.Outcome,r1.Outcome with
        | Timeout i,Timeout j when i = j -> true
        | Outcome.Exception _, Outcome.Exception _ -> true
        | False,False -> true 
        | True,True -> true
        | Rejected,Rejected -> true
        | _ -> false
        && List.for_all2 (fun s0 s1 -> s0 = s1) r0.Stamp r1.Stamp
        && Set.equal r0.Labels r1.Labels
        && List.for_all2 (fun s0 s1 -> s0 = s1) r0.Arguments r1.Arguments
    
    let rec private depth (prop:SymProp) =
        match prop with
        | Unit -> 0
        | Bool b -> 0
        | Exception -> 0
        | ForAll (_,prop) -> 1 + (depth prop)
        | Implies (_,prop) -> 1 + (depth prop)
        | Classify (_,_,prop) -> 1 + (depth prop)
        | Collect (_,prop) -> 1 + (depth prop)
        | Label (_,prop) -> 1 + (depth prop)
        | And (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | Or (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | Lazy prop -> 1 + (depth prop)
        | Tuple2 (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | Tuple3 (prop1,prop2,prop3) -> 1 + Math.Max(Math.Max(depth prop1, depth prop2),depth prop3)
        | List props -> 1 + List.fold_left (fun a b -> Math.Max(a, depth b)) 0 props
    
    let Property = 
        forAllShrink symPropGen shrink (fun symprop ->
            let expected = determineResult symprop
            let (MkRose (Common.Lazy actual,_)) = generate 1 (Random.newSeed()) (toProperty symprop) 
            areSame expected actual
            |> label (sprintf "expected = %A - actual = %A" expected actual)
            |> collect (depth symprop)
        )
            
        
        
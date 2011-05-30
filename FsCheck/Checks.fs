(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2010 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck.Checks

open FsCheck
open Gen
open Prop

module Helpers = 

    open System

    let sample n gn  = 
        let rec sample i seed samples =
            if i = 0 then samples
            else sample (i-1) (Random.stdSplit seed |> snd) (eval 1000 seed gn :: samples)
        sample n (Random.newSeed()) []

    let sample1 gn = sample 1 gn |> List.head
    
    let isIn l elem = List.exists ((=) elem) l

module Common = 

    open FsCheck.Common

    let Memoize (f:int->string) (a:int) = memoize f a = f a

    let Flip (f: char -> int -> string) a b = flip f a b = f b a
    
module Random =

    open FsCheck.Random
    open Helpers

    let ``abs(v) % k equals abs(v % k)`` v (NonZeroInt k) = 
        (abs v) % k = abs(v % k)


    let DivMod (x:int) (y:int) = 
        y <> 0 ==> lazy (let (d,m) = divMod x y in d*y + m = x)
        
    let MkStdGen (IntWithMinMax seed) =
        within 1000 <| lazy (let (StdGen (s1,s2)) = mkStdGen (int64 seed) in s1 > 0 && s2 > 0 (*todo:add check*) ) //check for bug: hangs when seed = min_int
        //|> collect seed

module Generator = 

    open Helpers
    
    let Choose (Interval (l,h)) = 
        choose (l,h)
        |> sample 10
        |> List.forall (fun v -> l <= v && v <= h)
     

       
    let Elements (l:list<char>) =
        not l.IsEmpty ==> 
        lazy (  elements l
                |> sample 50
                |> List.forall (isIn l))
    
    let Constant (v : char) =
        constant v
        |> sample 10
        |> List.forall ((=) v)
    
    let Oneof (l:list<string>) =
        not l.IsEmpty ==> 
        lazy (  List.map constant l
                |> oneof
                |> sample 10
                |> List.forall (isIn l))
    
    let Frequency (frequencies:list<PositiveInt*string>) =
        let generatedValues = frequencies |> List.filter (fst >> (fun p -> p.Get) >> (<>) 0) |> List.map snd
        (sprintf "%A" generatedValues) @|
        (not generatedValues.IsEmpty ==>
         lazy ( frequencies
                |> List.map (fun (freq,s) -> (freq.Get,constant s))
                |> frequency
                |> sample 100
                |> List.forall (isIn generatedValues)))
    
    let Map (f:string -> int) v =
        map f (constant v)
        |> sample 1
        |> List.forall ((=) (f v))
        
    let Map2 (f:char -> int -> int) a b =
        map2 f (constant a) (constant b)
        |> sample1
        |> ((=) (f a b))
        
    let Map3 (f:int -> char -> int -> int) a b c =
        map3 f (constant a) (constant b) (constant c)
        |> sample1
        |> ((=) (f a b c))
        
    let Map4 (f:char -> int -> char -> bool -> int) a b c d =
        map4 f (constant a) (constant b) (constant c) (constant d)
        |> sample1
        |> ((=) (f a b c d))
        
    let Map5 (f:bool -> char -> int -> char -> bool -> int) a b c d e =
        map5 f (constant a) (constant b) (constant c) (constant d) (constant e)
        |> sample1
        |> ((=) (f a b c d e))
        
    let Map6 (f:bool -> char -> int -> char -> bool -> int -> char ) a b c d e g = 
        map6 f (constant a) (constant b) (constant c) (constant d) (constant e) (constant g)
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
        
    let ListOfLength (v:char) (PositiveInt length) =
        listOfLength length (constant v)
        |> sample1
        |> ((=) (List.init length (fun _ -> v)))
    
    let SuchThatOption (v:int) (predicate:int -> bool) =
        let expected = if predicate v then Some v else None
        suchThatOption predicate (constant v)
        |> sample1
        |> ((=) expected)
//        |> classify expected.IsNone "None"
//        |> classify expected.IsSome "Some"
        
    let SuchThat (v:int) =
        suchThat ((<=) 0) (elements [v;abs v])
        |> sample1
        |> ((=) (abs v))
    
    let ListOf (NonNegativeInt size) (v:char) =
        resize size (listOf <| constant v)
        |> sample 10
        |> List.forall (fun l -> l.Length <= size+1 && List.forall ((=) v) l)
    
    let NonEmptyListOf (NonNegativeInt size) (v:string) =
        let actual = resize size (nonEmptyListOf <| constant v) |> sample 10
        actual
        |> List.forall (fun l -> 0 < l.Length && l.Length <= max 1 size && List.forall ((=) v) l) 
        //|> label (sprintf "Actual: %A" actual)
    
    let SubListOf (l:list<int>) =
        subListOf l
        |> sample 10
        |> List.forall (fun sublist -> 
            List.length sublist <= List.length l
            && List.forall (fun e -> List.exists ((=) e) l) sublist)

    let ArrayOf (NonNegativeInt size) (v:int) =
        resize size (arrayOf <| constant v)
        |> sample 10
        |> List.forall (fun l -> l.Length <= size+1 && Array.forall ((=) v) l)
    
    let ArrayOfLength (v:char) (PositiveInt length) =
        arrayOfLength length (constant v)
        |> sample1
        |> ((=) (Array.init length (fun _ -> v)))
    
    let Array2DOf (NonNegativeInt size) (v:int) =
        resize size (array2DOf (constant v))
        |> sample1
        |> fun arr ->
            (arr.Length <= size+1) 
            && (seq { for elem in arr do yield elem :?> int} |> Seq.forall ((=) v))
  
    let Array2DOfDim (NonNegativeInt rows,NonNegativeInt cols) (v:int) =
        (array2DOfDim (rows,cols) (constant v))
        |> sample1
        |> fun arr ->
            (Array2D.length1 arr <= rows) 
            && (Array2D.length2 arr <= cols) 
            && (seq { for elem in arr do yield elem :?> int} |> Seq.forall ((=) v))

    //variant generators should be independent...this is not a good check for that.
    let Variant (v:char) =
        variant v (constant v) |> sample1 |>  ((=) v)
          
module Arbitrary =
    
    open FsCheck
    open System
    open Helpers
    open Arb
    
    let private addLabels (generator,shrinker) = ( generator |@ "Generator", shrinker |@ "Shrinker")
    
    let Unit() = 
        (   generate<unit> |> sample 10 |> List.forall ((=) ())
        ,   shrink<unit>() |> Seq.isEmpty)
        |> addLabels
    
    let Boolean (b:bool) =
        (   generate<bool> |> sample 10 |> List.forall (fun _ -> true)
        ,    shrink<bool> b |> Seq.isEmpty)
        |> addLabels
    
    let Int32 (NonNegativeInt size) (v:int) =
        (   generate<int> |> resize size |> sample 10 |> List.forall (fun v -> -size <= v && v <= size)
        ,   shrink<int> v |> Seq.forall (fun shrunkv -> shrunkv <= abs v))
            
    let Double (NonNegativeInt size) (value:float) =
        (   generate<float> |> resize size |> sample 10
            |> List.forall (fun v -> 
                (-2.0 * float size <= v && v <= 2.0 * float size )
                || Double.IsNaN(v) || Double.IsInfinity(v)
                || v = Double.Epsilon || v = Double.MaxValue || v = Double.MinValue)
        ,   shrink<float> value 
            |> Seq.forall (fun shrunkv -> shrunkv = 0.0 || shrunkv <= abs value))
        |> addLabels
        
    let Byte (value:byte) =
        (   generate<byte> |> sample 10 |> List.forall (fun _ -> true) //just check that we can generate bytes
        ,   shrink<byte> value |> Seq.forall (fun shrunkv -> (int shrunkv) <= abs (int value)))

    let Char (value:char) =
        (   generate<char> |> sample 10 |> List.forall (fun v -> v >= Char.MinValue && (int v) <= 127)
        ,   shrink<char> value |> Seq.forall (fun shrunkv -> isIn  ['a';'b';'c'] shrunkv ))

    let String (value:string) =
        (   generate<string> |> sample 10 |> List.forall (fun _ -> true)
            //or the lenght of the string is shorter, or one of its values have been shrunk
        ,   shrink<string> value |> Seq.forall (fun s -> String.length s < String.length value || (String.exists (isIn ['a';'b';'c']) s)) )
        |> addLabels
      
    let ``2-Tuple``((valuei:int,valuec:char) as value) =
        (   generate<int*char> |> sample 10 |> List.forall (fun _ -> true)
            //or the first value is shrunk, or the second
        ,   shrink value |> Seq.forall (fun (i,c) -> shrink valuei |> Seq.exists ((=) i) || shrink valuec |> Seq.exists ((=) c))  )
    
    let ``3-Tuple``((valuei:int,valuec:char,valueb:bool) as value) =
        (   generate<int*char*bool> |> sample 10 |> List.forall (fun _ -> true)
            //or the first value is shrunk, or the second, or the third
        ,   shrink value |> Seq.forall (fun (i,c,b) -> shrink valuei |> Seq.exists ((=) i) 
                                                    || shrink valuec |> Seq.exists ((=) c)  
                                                    || shrink valueb |> Seq.exists ((=) b))   )
     
    let Option (value:option<int>) =
        (   generate<option<int>> |> sample 10 |> List.forall (fun _ -> true)
        ,   shrink value 
            |> (fun shrinks -> match value with 
                                | None -> shrinks = Seq.empty 
                                | Some v ->  Seq.forall2 (=) shrinks (seq { yield None; for x' in shrink v -> Some x' }) ))
      
    let Function (f:int->char) (vs:list<int>) =
        let tabledF = Function<_,_>.from f
        (List.map tabledF.Value vs) = (List.map f vs)
        && List.forall (fun v -> List.tryFind (fst >> (=) v) tabledF.Table = Some (v,f v)) vs
    
    //checks that a generated function is pure by applying it twice to the same values and checking that the results are the same.
    let FunctionIsPure (f:int->char->bool) (vs:list<int*char>) =
        List.forall2 (=)
            (List.map (Common.uncurry f) vs)
            (List.map (Common.uncurry f) vs)
    
    let Object (o:Object) =
        let goodObject (o:obj) = 
            match o with
            | :? char | :? bool | :? string -> true
            | _ -> false
        let goodShrinks (o:obj) shrinks = Seq.forall2 (=) (shrink (unbox o)) (shrinks |> Seq.map unbox)
        ( goodObject o
        , shrink o |> goodShrinks o)
            
    let ``DateTime generates year between 1900 and 2100``(value:DateTime) =
        1900 <= value.Year && value.Year <= 2100

    let ``DateTime shrinks seconds, minutes and hours`` (value:DateTime) =
        shrink value
        |> Seq.forall (fun v -> v.Second = 0 
                                 || (v.Second = 0 && v.Minute = 0) 
                                 || (v.Second = 0 && v.Minute = 0 && v.Hour = 0) )
                                 
    let ``Array shrinks to shorter array or smaller elements`` (value:int[]) =
        shrink value 
        |> Seq.forall (fun v -> v.Length < value.Length || Array.exists2 (fun e1 e2 -> abs e1 <= abs e2) v value)
        |> Prop.label (sprintf "%A" <| shrink value)
        
    let ``Array2D shrinks to smaller array or smaller elements`` (value:int[,]) =
        let existsSmallerElement arr v = 
            let result = ref false
            Array2D.iteri (fun i j elem -> result := (!result || abs elem <= abs value.[i,j])) v
            !result
        shrink value 
        |> Seq.forall (fun v -> 
                Array2D.length1 v < Array2D.length1 value
                || Array2D.length2 v < Array2D.length2 value
                || existsSmallerElement value v)
                
    let ``NonNegativeInt generates non negative ints`` (NonNegativeInt value) =
        value >= 0
        
    let ``NonNegativeInt shrinks non negative ints`` (value:NonNegativeInt) =
        shrink value |> Seq.forall (fun (NonNegativeInt v) -> v >= 0)
        
    let ``PositiveInt generates positive ints`` (PositiveInt value) =
        value > 0
        
    let ``PositiveInt shrinks positive ints`` (value:PositiveInt ) =
        shrink value |> Seq.forall (fun (PositiveInt v) -> v > 0)
    
        
module Property =
    open FsCheck.Prop
    open FsCheck.Common
    open System
    open Arb
    
    type SymProp =  | Unit | Bool of bool | Exception
                    | ForAll of int * SymProp
                    | Implies of bool * SymProp
                    | Classify of bool * string * SymProp
                    | Collect of int * SymProp
                    | Label of string * SymProp
                    | And of SymProp * SymProp
                    | Or of SymProp * SymProp
                    | LazyProp of SymProp
                    | Tuple2 of SymProp * SymProp
                    | Tuple3 of SymProp * SymProp * SymProp //and 4,5,6
                    | List of SymProp list
     
    let rec private symPropGen =
        let rec recGen size =
            match size with
            | 0 -> oneof [constant Unit; map (Bool) generate; constant Exception]
            | n when n>0 ->
                let subProp = recGen (size/2)
                oneof   [ map2 (curry ForAll) generate (subProp)
                        ; map2 (curry Implies) generate (subProp)
                        ; map2 (curry Collect) generate (subProp)
                        ; map3 (curry2 Classify) generate generate (subProp)
                        ; map2 (curry Label) generate (subProp)
                        ; map2 (curry And) (subProp) (subProp)
                        ; map2 (curry Or) (subProp) (subProp)
                        ; map LazyProp subProp
                        ; map2 (curry Tuple2) subProp subProp
                        ; map3 (curry2 Tuple3) subProp subProp subProp
                        ; map List (resize 3 <| nonEmptyListOf subProp)
                        ]
            | _ -> failwith "symPropGen: size must be positive"
        sized recGen
                  
    let rec private determineResult prop =
        let addStamp stamp res = { res with Stamp = stamp :: res.Stamp }
        let addArgument arg res = { res with Arguments = arg :: res.Arguments }
        let addLabel label (res:Result) = { res with Labels = Set.add label res.Labels }
        let andCombine prop1 prop2 :Result = let (r1:Result,r2) = determineResult prop1, determineResult prop2 in r1 &&& r2
        match prop with
        | Unit -> Res.succeeded
        | Bool true -> Res.succeeded
        | Bool false -> Res.failed
        | Exception  -> Res.exc <| InvalidOperationException()
        | ForAll (i,prop) -> determineResult prop |> addArgument i
        | Implies (true,prop) -> determineResult prop
        | Implies (false,_) -> Res.rejected
        | Classify (true,stamp,prop) -> determineResult prop |> addStamp stamp
        | Classify (false,_,prop) -> determineResult prop
        | Collect (i,prop) -> determineResult prop |> addStamp (sprintf "%A" i)
        | Label (l,prop) -> determineResult prop |> addLabel l
        | And (prop1, prop2) -> andCombine prop1 prop2
        | Or (prop1, prop2) -> let r1,r2 = determineResult prop1, determineResult prop2 in r1 ||| r2
        | LazyProp prop -> determineResult prop
        | Tuple2 (prop1,prop2) -> andCombine prop1 prop2
        | Tuple3 (prop1,prop2,prop3) -> (andCombine prop1 prop2) &&& (determineResult prop3)
        | List props -> List.fold (fun st p -> st &&& determineResult p) (List.head props |> determineResult) (List.tail props)
        
    let rec private toProperty prop =
        match prop with
        | Unit -> Testable.property ()
        | Bool b -> Testable.property b
        | Exception -> Testable.property (lazy (raise <| InvalidOperationException()))
        | ForAll (i,prop) -> forAll (constant i |> Arb.fromGen) (fun i -> toProperty prop)
        | Implies (b,prop) -> b ==> (toProperty prop)
        | Classify (b,stamp,prop) -> classify b stamp (toProperty prop)
        | Collect (i,prop) -> collect i (toProperty prop)
        | Label (l,prop) -> label l (toProperty prop)
        | And (prop1,prop2) -> (toProperty prop1) .&. (toProperty prop2)
        | Or (prop1,prop2) -> (toProperty prop1) .|. (toProperty prop2)
        | LazyProp prop -> toProperty prop
        | Tuple2 (prop1,prop2) -> (toProperty prop1) .&. (toProperty prop2)
        | Tuple3 (prop1,prop2,prop3) -> (toProperty prop1) .&. (toProperty prop2) .&. (toProperty prop3)
        | List props -> List.fold (fun st p -> st .&. toProperty p) (List.head props |> toProperty) (List.tail props)
    
    let private areSame (r0:Result) (r1:Result) =
        match r0.Outcome,r1.Outcome with
        | Timeout i,Timeout j when i = j -> true
        | Outcome.Exception _, Outcome.Exception _ -> true
        | Outcome.False,Outcome.False -> true 
        | Outcome.True,Outcome.True -> true
        | Rejected,Rejected -> true
        | _ -> false
        && List.forall2 (fun s0 s1 -> s0 = s1) r0.Stamp r1.Stamp
        && r0.Labels = r1.Labels
        && List.forall2 (fun s0 s1 -> s0 = s1) r0.Arguments r1.Arguments
    
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
        | LazyProp prop -> 1 + (depth prop)
        | Tuple2 (prop1,prop2) -> 1 + Math.Max(depth prop1, depth prop2)
        | Tuple3 (prop1,prop2,prop3) -> 1 + Math.Max(Math.Max(depth prop1, depth prop2),depth prop3)
        | List props -> 1 + List.fold (fun a b -> Math.Max(a, depth b)) 0 props
    
    let DSL() = 
        forAll (Arb.fromGenShrink(symPropGen,shrink)) (fun symprop ->
            let expected = determineResult symprop
            let (MkRose (Lazy actual,_)) = eval 1 (Random.newSeed()) (toProperty symprop) 
            areSame expected actual
            |> label (sprintf "expected = %A - actual = %A" expected actual)
            |> collect (depth symprop)
        )
            
        
        
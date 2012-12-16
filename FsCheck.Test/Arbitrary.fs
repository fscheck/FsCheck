
namespace FsCheck.Test

module Arbitrary =
    
    open FsCheck
    open FsCheck.Xunit
    open global.Xunit
    open System
    open System.Collections.Generic
    open Helpers
    open Arb
    
    let private addLabels (generator,shrinker) = ( generator |@ "Generator", shrinker |@ "Shrinker")
    
    [<Property>]
    let Unit() = 
        (   generate<unit> |> sample 10 |> List.forall ((=) ())
        ,   shrink<unit>() |> Seq.isEmpty)
        |> addLabels
    
    [<Property>]
    let Boolean (b:bool) =
        (   generate<bool> |> sample 10 |> List.forall (fun _ -> true)
        ,    shrink<bool> b |> Seq.isEmpty)
        |> addLabels
    
    [<Property>]
    let Int16 (NonNegativeInt size) (v:int16) =
        (   generate<int16> |> Gen.resize size |> sample 10 |> List.forall (fun v -> -size <= int v && int v <= size)
        ,   shrink<int16> v |> Seq.forall (fun shrunkv -> shrunkv <= abs v))

    [<Property>]
    let DontSizeInt16 (DontSize v as dontSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        (   generate<DontSize<int16>> |> Gen.resize 0 |> sample 100 |> List.exists (fun (DontSize v) -> v <> 0s)
        ,   shrink<DontSize<int16>> dontSizeV |> Seq.forall (fun (DontSize shrunkv) -> shrunkv <= abs v))

    [<Property>]
    let Int32 (NonNegativeInt size) (v:int) =
        (   generate<int> |> Gen.resize size |> sample 10 |> List.forall (fun v -> -size <= v && v <= size)
        ,   shrink<int> v |> Seq.forall (fun shrunkv -> shrunkv <= abs v))

    [<Property>]
    let DontSizeInt32 (DontSize v as dontSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        (   generate<DontSize<int>> |> Gen.resize 0 |> sample 100 |> List.exists (fun (DontSize v) -> v <> 0)
        ,   shrink<DontSize<int>> dontSizeV |> Seq.forall (fun (DontSize shrunkv) -> shrunkv <= abs v))

    [<Property>]
    let Int64 (NonNegativeInt size) (value: int64) = 
        (   generate<int64> |> Gen.resize size |> sample 10 |> List.forall (fun v -> -(int64 size) <= v && v <= int64 size)
        ,   shrink<int64> value |> Seq.forall (fun shrunkv -> (int shrunkv) <= abs (int value)))
                
    [<Property>]
    let DontSizeInt64 (DontSize v as dontSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        (   generate<DontSize<int64>> |> Gen.resize 0 |> sample 100 |> List.exists (fun (DontSize v) -> v <> 0L)
        ,   shrink<DontSize<int64>> dontSizeV |> Seq.forall (fun (DontSize shrunkv) -> shrunkv <= abs v))

    [<Property>]
    let Double (NonNegativeInt size) (value:float) =
        (   generate<float> |> Gen.resize size |> sample 10
            |> List.forall (fun v -> 
                (-2.0 * float size <= v && v <= 2.0 * float size )
                || Double.IsNaN(v) || Double.IsInfinity(v)
                || v = Double.Epsilon || v = Double.MaxValue || v = Double.MinValue)
        ,   shrink<float> value 
            |> Seq.forall (fun shrunkv -> shrunkv = 0.0 || shrunkv <= abs value))
        |> addLabels
        
    [<Property>]
    let Byte (value:byte) =
        (   generate<byte> |> sample 10 |> List.forall (fun _ -> true) //just check that we can generate bytes
        ,   shrink<byte> value |> Seq.forall (fun shrunkv -> (int shrunkv) <= abs (int value)))
        
    [<Property>]
    let SByte (value:sbyte) =
        (   generate<sbyte> |> sample 10 |> List.forall (fun _ -> true) //just check that we can generate sbytes
        ,   shrink<sbyte> value |> Seq.forall (fun shrunkv -> shrunkv <= abs value ))

    [<Property>]
    let Char (value:char) =
        (   generate<char> |> sample 10 |> List.forall (fun v -> v >= Char.MinValue && (int v) <= 127)
        ,   shrink<char> value |> Seq.forall (fun shrunkv -> isIn  ['a';'b';'c'] shrunkv ))

    [<Property>]
    let String (value:string) =
        (   generate<string> |> sample 10 |> List.forall (fun _ -> true)
            //or the lenght of the string is shorter, or one of its values have been shrunk
        ,   shrink<string> value |> Seq.forall (fun s -> String.length s < String.length value || (String.exists (isIn ['a';'b';'c']) s)) )
        |> addLabels
      
    [<Property>]
    let ``2-Tuple``((valuei:int,valuec:char) as value) =
        (   generate<int*char> |> sample 10 |> List.forall (fun _ -> true)
            //or the first value is shrunk, or the second
        ,   shrink value |> Seq.forall (fun (i,c) -> shrink valuei |> Seq.exists ((=) i) || shrink valuec |> Seq.exists ((=) c))  )
    
    [<Property>]
    let ``3-Tuple``((valuei:int,valuec:char,valueb:bool) as value) =
        (   generate<int*char*bool> |> sample 10 |> List.forall (fun _ -> true)
            //or the first value is shrunk, or the second, or the third
        ,   shrink value |> Seq.forall (fun (i,c,b) -> shrink valuei |> Seq.exists ((=) i) 
                                                    || shrink valuec |> Seq.exists ((=) c)  
                                                    || shrink valueb |> Seq.exists ((=) b))   )
     
    [<Property>]
    let Option (value:option<int>) =
        (   generate<option<int>> |> sample 10 |> List.forall (fun _ -> true)
        ,   shrink value 
            |> (fun shrinks -> match value with 
                                | None -> shrinks = Seq.empty 
                                | Some v ->  Seq.forall2 (=) shrinks (seq { yield None; for x' in shrink v -> Some x' }) ))

    let testFunction (f: _ -> _) (vs: _ list) =
        let tabledF = Function<_,_>.from f
        (List.map tabledF.Value vs) = (List.map f vs)
        && List.forall (fun v -> List.tryFind (fst >> (=) v) tabledF.Table = Some (v,f v)) vs
        
    [<Property>]  
    let Function (f:int->char) (vs:list<int>) =
        testFunction f vs
    
    [<Property>]
    //checks that a generated function is pure by applying it twice to the same values and checking that the results are the same.
    let FunctionIsPure (f:int->char->bool) (vs:list<int*char>) =
        List.forall2 (=)
            (List.map (fun (a,b) -> f a b) vs)
            (List.map (fun (a,b) -> f a b) vs)

    [<Property>]
    let SystemFunc (f: Func<int>) (vs: list<unit>) =
        testFunction f.Invoke vs

    [<Property>]
    let SystemFunc1 (f: Func<int, string>) (vs: list<int>) =
        testFunction f.Invoke vs

    [<Property>]
    let SystemFunc2 (f: Func<int, string, string>) (vs: list<int * string>) =
        testFunction f.Invoke vs

    [<Property>]
    let SystemAction2 (f: Action<int, string>) (vs: list<int * string>) =
        testFunction f.Invoke vs
            
    [<Property>]
    let Object (o:Object) =
        let goodObject (o:obj) = 
            match o with
            | :? char | :? bool | :? string -> true
            | _ -> false
        let goodShrinks (o:obj) shrinks = Seq.forall2 (=) (shrink (unbox o)) (shrinks |> Seq.map unbox)
        ( goodObject o
        , shrink o |> goodShrinks o)
            
    [<Property>]
    let ``DateTime generates year between 1900 and 2100``(value:DateTime) =
        1900 <= value.Year && value.Year <= 2100

    [<Property>]
    let ``DateTime shrinks seconds, minutes and hours`` (value:DateTime) =
        shrink value
        |> Seq.forall (fun v -> v.Second = 0 
                                 || (v.Second = 0 && v.Minute = 0) 
                                 || (v.Second = 0 && v.Minute = 0 && v.Hour = 0) )

    [<Fact>]
    let ``TimeSpan``() =
        generate<TimeSpan> |> sample 10 |> List.forall (fun _ -> true)

    [<Property>]
    let ``TimeSpan shrinks`` (value: TimeSpan) =
        shrink value
        |> Seq.forall (fun v -> v.Days = 0
                                || (v.Days = 0 && v.Hours = 0)
                                || (v.Days = 0 && v.Hours = 0 && v.Minutes = 0)
                                || (v.Days = 0 && v.Hours = 0 && v.Minutes = 0 && v.Seconds = 0)
                                || (v.Days = 0 && v.Hours = 0 && v.Minutes = 0 && v.Seconds = 0 || v.Milliseconds = 0))

    [<Fact>]
    let KeyValuePair () =
        generate<KeyValuePair<int,int>> |> sample 10 |> List.forall (fun _ -> true)

    [<Property>]
    let ``KeyValuePair shrinks`` (value: KeyValuePair<int, int>) =
        shrink value 
        |> Seq.forall (fun (KeyValue(k,v)) -> shrink value.Key |> Seq.exists ((=) k) || shrink value.Value |> Seq.exists ((=) v))

    [<Property>]                       
    let ``Array shrinks to shorter array or smaller elements`` (value:int[]) =
        shrink value 
        |> Seq.forall (fun v -> v.Length < value.Length || Array.exists2 (fun e1 e2 -> abs e1 <= abs e2) v value)
        |> Prop.label (sprintf "%A" <| shrink value)
        
    [<Property>]
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
                
    [<Property>]
    let ``NonNegativeInt generates non negative ints`` (NonNegativeInt value) =
        value >= 0
        
    [<Property>]
    let ``NonNegativeInt shrinks non negative ints`` (value:NonNegativeInt) =
        shrink value |> Seq.forall (fun (NonNegativeInt v) -> v >= 0)

    [<Property>]
    let ``PositiveInt generates positive ints`` (PositiveInt value) =
        value > 0
       
    [<Property>]
    let ``PositiveInt shrinks positive ints`` (value:PositiveInt ) =
        shrink value |> Seq.forall (fun (PositiveInt v) -> v > 0)
    
    type TestEnum =
        | A = 0
        | B = 3
        | C = 4

    [<Property>]
    let Enum  (value:TestEnum) =
        List.exists (fun e -> e = int value) [0;3;4]

    [<Flags>]
    type TestFlags =
        | A = 1
        | B = 2
        | C = 4

    [<Property>]
    let Flags (value:TestFlags) =
        List.exists (fun e -> e = int value) [0;1;2;3;4;5;6;7]

    [<Fact>]
    let ``Generic List``() =
        generate<List<int>> |> sample 10 |> List.forall (fun _ -> true)

    [<Fact>]
    let ``Generic IList``() =
        generate<IList<int>> |> sample 10 |> List.forall (fun _ -> true)

    [<Property>]
    let ``Generic IList shrinks`` (value: int IList) =
        shrink value 
        |> Seq.forall (fun l -> l.Count <= value.Count)

    [<Fact>]
    let ``Generic ICollection``() =
        generate<ICollection<int>> |> sample 10 |> List.forall (fun _ -> true)

    [<Property>]
    let ``Generic ICollection shrinks`` (value: int ICollection) =
        shrink value 
        |> Seq.forall (fun l -> l.Count <= value.Count)

    [<Fact>]
    let ``Generic Dictionary``() =
        generate<Dictionary<int, char>> |> sample 10 |> List.forall (fun _ -> true)

    [<Property>]
    let ``Generic Dictionary shrinks`` (value: Dictionary<int, string>) =
        shrink value 
        |> Seq.forall (fun l -> l.Count < value.Count)

    [<Fact>]
    let ``Generic IDictionary``() =
        generate<IDictionary<int, char>> |> sample 10 |> List.forall (fun _ -> true)

    [<Property>]
    let Decimal() =
        generate<decimal> |> sample 10 |> List.forall (fun _ -> true)

    [<Property>]
    let ``Decimal shrinks`` (value: decimal) =
        shrink<decimal> value 
        |> Seq.forall (fun shrunkv -> shrunkv = 0m || shrunkv <= abs value)
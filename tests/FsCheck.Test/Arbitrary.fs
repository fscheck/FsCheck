
namespace FsCheck.Test

module Arbitrary =

    open Xunit    
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open System
    open System.Globalization
    open System.Collections.Generic
    open System.Net
    open System.Net.Mail
    open Helpers

    open Swensen.Unquote

    let arbitrary<'T> = ArbMap.defaults |> ArbMap.arbitrary<'T>
        
    let sample n = Arb.sampleWithSize 1000 n
    
    let sample1 gn = sample 1 gn |> Seq.head

    let generate<'T> = arbitrary<'T>
    let shrink<'T> = arbitrary<'T> |> Arb.shrinkSample
    
    [<Fact>]
    let Unit() = 
        assertTrue ( generate<unit> |> sample 10 |> Seq.forall ((=) ()) )
        assertTrue ( shrink<unit> |> snd |> Seq.isEmpty )
    
    [<Fact>]
    let Boolean() =
        assertTrue ( generate<bool> |> sample 10 |> Seq.forall (fun _ -> true) )
        assertTrue ( shrink<bool> |> fun (sample,shrinks) -> if sample then Seq.toList shrinks = [false] else Seq.isEmpty shrinks)
    
    [<Property>]
    let Int16 (NonNegativeInt size) =
        assertTrue ( generate<int16> |> Arb.resize size |> sample 10 |> Seq.forall (fun v -> -size <= int v && int v <= size) )
        assertTrue ( shrink<int16> |> fun (v,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> shrunkv <= abs v) )

    [<Fact>]
    let DoNotSizeInt16 () =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue ( generate<DoNotSize<int16>> |> Arb.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0s) )
        assertTrue ( shrink<DoNotSize<int16>> |> fun (DoNotSize v, shrinks) -> shrinks |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= abs v) )

    [<Property>]
    let UInt16 (NonNegativeInt size) =
        assertTrue (  generate<uint16> |> Arb.resize size |> sample 10 |> Seq.forall (fun v -> int v <= size) )
        assertTrue (  shrink<uint16> |> fun (v,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> shrunkv <= v) )

    [<Fact>]
    let DoNotSizeUInt16 () =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue (  generate<DoNotSize<uint16>> |> Arb.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0us) )
        assertTrue (  shrink<DoNotSize<uint16>> |> fun (DoNotSize v, shrinks) -> shrinks |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= v) )

    [<Property>]
    let Int32 (NonNegativeInt size) =
        assertTrue (  generate<int> |> Arb.resize size |> sample 10 |> Seq.forall (fun v -> -size <= v && v <= size) )
        assertTrue (  shrink<int> |> fun (v, shrinks) -> shrinks |> Seq.forall (fun shrunkv -> shrunkv <= abs v) )

    [<Fact>]
    let DoNotSizeInt32 () =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue (  generate<DoNotSize<int>> |> Arb.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0) )
        assertTrue (  shrink<DoNotSize<int>> |> fun (DoNotSize v, shrinks) -> shrinks |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= abs v) )

    [<Property>]
    let UInt32 (NonNegativeInt size) =
        assertTrue ( generate<uint32> |> Arb.resize size |> sample 10 |> Seq.forall (fun v -> int v <= size) )
        assertTrue ( shrink<uint32> |> fun (v,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> shrunkv <= v) )

    [<Fact>]
    let DoNotSizeUInt32 () =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue (  generate<DoNotSize<uint32>> |> Arb.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0u) )
        assertTrue (  shrink<DoNotSize<uint32>> |> fun (DoNotSize v,shrinks) -> shrinks |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= v) )

    [<Property>]
    let Int64 (NonNegativeInt size) = 
        assertTrue ( generate<int64> |> Arb.resize size |> sample 10 |> Seq.forall (fun v -> -(int64 size) <= v && v <= int64 size) )
        assertTrue ( shrink<int64> |> fun (value,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> (int shrunkv) <= abs (int value)) )
                
    [<Fact>]
    let DoNotSizeInt64 () =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue ( generate<DoNotSize<int64>> |> Arb.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0L) )
        assertTrue ( shrink<DoNotSize<int64>> |> fun (DoNotSize v,shrinks) -> shrinks |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= abs v) )

    [<Property>]
    let UInt64 (NonNegativeInt size) =
        assertTrue ( generate<uint64> |> Arb.resize size |> sample 10 |> Seq.forall (fun v -> int v <= size) )
        assertTrue ( shrink<uint64> |> fun (v,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> shrunkv <= v) )

    [<Fact>]
    let DoNotSizeUInt64 () =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue ( generate<DoNotSize<uint64>> |> Arb.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0UL) )
        assertTrue ( shrink<DoNotSize<uint64>> |> fun (DoNotSize v,shrinks) -> shrinks |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= v) )

    //[<Property>]
    //let Double (NonNegativeInt size) (value:float) =
    //    assertTrue ( generate<float> |> Gen.resize size |> sample 10
    //            |> Seq.forall (fun v -> 
    //                (float -size <= v && v <= float size )
    //                || Double.IsNaN(v) || Double.IsInfinity(v)
    //                || v = Double.Epsilon || v = Double.MaxValue || v = Double.MinValue) )
    //    assertTrue ( shrink<float> value 
    //            |> Seq.forall (fun shrunkv -> shrunkv = 0.0 || shrunkv <= abs value) )
        
    //[<Property>]
    //let Single (NonNegativeInt size) (value:float32) =
    //    assertTrue ( generate<float32> |> Gen.resize size |> sample 10
    //            |> Seq.forall (fun v -> 
    //                (float32 -size <= v && v <= float32 size )
    //                || Single.IsNaN(v) || Single.IsInfinity(v)
    //                || v = Single.Epsilon || v = Single.MaxValue || v = Single.MinValue) )
    //    assertTrue ( shrink<float32> value 
    //            |> Seq.forall (fun shrunkv -> shrunkv = 0.0f || shrunkv <= abs value) )

    [<Fact>]
    let Byte () =
        assertTrue ( generate<byte> |> sample 10 |> Seq.forall (fun _ -> true) ) //just check that we can generate bytes
        assertTrue ( shrink<byte> |> fun (value,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> (int shrunkv) <= abs (int value)) )
        
    [<Fact>]
    let SByte () =
        assertTrue ( generate<sbyte> |> sample 10 |> Seq.forall (fun _ -> true) ) //just check that we can generate sbytes
        assertTrue ( shrink<sbyte> |> fun (value,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> int shrunkv <= abs (int value) ) )

    [<Fact>]
    let Char () =
        assertTrue ( generate<char> |> sample 10 |> Seq.forall (fun v -> v >= Char.MinValue && (int v) <= 127) )
        assertTrue ( shrink<char> |> fun (value, shrinks) -> shrinks |> Seq.forall (fun shrunkv -> shrunkv < value ) )

    [<Fact>]
    let String () =
        assertTrue ( generate<string> |> sample 10 |> Seq.forall (fun _ -> true) )
            //or the length of the string is shorter, or one of its values have been shrunk
        assertTrue ( shrink<string> |> fun (value, shrinks) -> shrinks |> Seq.forall (fun s -> s = null || String.length s < String.length value || (String.exists (isIn ['a';'b';'c']) s))  )

    [<Property>]
    let ``Non-empty string`` (NonEmptyString v) = 
        not (System.String.IsNullOrEmpty v)
        |> assertTrue
      
    [<Property>]
    let ``Non-whitespace string`` (NonWhiteSpaceString s) = not (System.String.IsNullOrWhiteSpace s)

    [<Property>]
    let ``XML encoded string is serializable`` (XmlEncodedString value) =
        let doc = System.Xml.XmlDocument()
        doc.LoadXml (sprintf "<Root>%s</Root>" value)

    [<Fact>]
    let ``2-Tuple``() =
        assertTrue ( generate<int*char> |> sample 10 |> Seq.forall (fun _ -> true) )
            //or the first value is shrunk, or the second
        assertTrue ( shrink<int*char> 
                     |> fun ((valuei,valuec), shrinks) -> shrinks |> Seq.forall (fun (i,c) -> valuei = i || valuec = c) )
    
    [<Fact>]
    let ``3-Tuple``() =
        assertTrue ( generate<int*char*bool> |> sample 10 |> Seq.forall (fun _ -> true) )
            //or the first value is shrunk, or the second, or the third
        assertTrue ( shrink<int*char*bool>
                     |> fun ((valuei,valuec,valueb), shrinks) -> 
                            shrinks |> Seq.forall (fun (i,c,b) -> valuei = i || valuec = c || valueb = b))
     
    [<Fact>]
    let Option () =
        generate<option<int>> |> sample 10 |> ignore
        assertTrue ( shrink<option<int>> |> fun (value, shrinks) ->
                        match value with 
                        | None -> Seq.isEmpty shrinks
                        | Some v -> Seq.head shrinks = None )

    //[<Fact>]
    //let NonNull () = 
    //    assertTrue ( generate<NonNull<string>> |> sample 10 |> Seq.forall (fun (NonNull x) -> not (isNull x)) )
    //    assertTrue ( shrink<NonNull<string>> |> snd |> Seq.forall (fun (NonNull x) -> not (isNull x)) )

    [<Fact>]
    let Nullable () =
        //generate<Nullable<int>> |> sample 10 |> ignore
        assertTrue ( shrink<Nullable<int>> |> fun (value, shrinks) ->
                        if value.HasValue then 
                            Seq.head shrinks = Nullable()
                        else 
                            Seq.isEmpty shrinks )

    //let testFunction (f: _ -> _) (vs: _ list) =
    //    let tabledF = Function<_,_>.From f
    //    assertTrue (List.map tabledF.Value vs = List.map f vs)
    //    assertTrue (List.forall (fun v -> List.tryFind (fst >> (=) v) tabledF.Table = Some (v,f v)) vs)
        
    //[<Property>]
    //let Function (f:int->int) (vs:list<int>) =
    //    testFunction f vs
    
    //[<Property>]
    ////checks that a generated function is pure by applying it twice to the same values and checking that the results are the same.
    //let FunctionIsPure (f:int->char->bool) (vs:list<int*char>) =
    //    assertTrue <| List.forall2 (=)
    //        (List.map (fun (a,b) -> f a b) vs)
    //        (List.map (fun (a,b) -> f a b) vs)

    //[<Property>]
    //let ``Fun pattern works``(Fun (f:bool->bool)) =
    //    f true |> ignore
    //    f false |> ignore
    //    ()

    //[<Property>]
    //let ``ThrowingFunction throws exceptions from list`` (vs:list<int>) =
    //    let exceptions : Exception list = 
    //        [ NullReferenceException("exc1")
    //          ArgumentNullException("exc2") ]
    //    let catch f v = 
    //        try
    //            f v |> ignore
    //        with 
    //        | :? NullReferenceException as e -> assertTrue (e.Message = "exc1")
    //        | :? ArgumentNullException as e -> assertTrue (e.ParamName = "exc2")

    //    Prop.forAll (arbitrary |> Arb.throwingFunction<int,int> exceptions) (fun f -> vs |> Seq.iter (catch f))

    //[<Property>]
    //let ``ThrowingFunction throws exceptions`` (vs:list<int>) (ThrowingFunction f) =
    //    let catch f v = 
    //        try
    //            f v |> ignore<int>
    //            ()
    //        with 
    //        | _ -> ()

    //    vs |> Seq.iter (catch f)

    //[<Property>]
    //let SystemFunc (f: Func<int>) (vs: list<unit>) =
    //    testFunction f.Invoke vs

    //[<Property>]
    //let SystemFunc1 (f: Func<int, string>) (vs: list<int>) =
    //    testFunction f.Invoke vs

    //[<Property>]
    //let SystemFunc2 (f: Func<int, string, string>) (vs: list<int * string>) =
    //    testFunction f.Invoke vs

    //[<Property>]
    //let SystemAction2 (f: Action<int, string>) (vs: list<int * string>) =
    //    testFunction f.Invoke vs
            
    //[<Property>]
    //let Object (o:Object) =
    //    let goodObject (o:obj) = 
    //        match o with
    //        | null | :? char | :? bool | :? string -> true
    //        | _ -> false
    //    let goodShrinks (o:obj) shrinks = Seq.forall2 (=) (shrink (unbox o)) (shrinks |> Seq.map unbox)
    //    assertTrue (goodObject o)
    //    assertTrue (shrink o |> goodShrinks o)
            
    //[<Property>]
    //let ``DateTime generates year between 1900 and 2100``(value:DateTime) =
    //    assertTrue (1900 <= value.Year)
    //    assertTrue (value.Year <= 2100)

    //[<Property>]
    //let ``DateTime shrinks incrementally remove kind and time components`` (value:DateTime) =
    //    let rec checkShrink (value:DateTime) =
    //        let shrinks = shrink value
    //        if value.Kind <> DateTimeKind.Unspecified then
    //            shrinks |> Seq.forall (fun v -> v.Kind = DateTimeKind.Unspecified && checkShrink v)
    //        elif value.Millisecond <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Kind = DateTimeKind.Unspecified && v.Millisecond = 0 && checkShrink v)
    //        elif value.Second <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Kind = DateTimeKind.Unspecified && v.Millisecond = 0 && v.Second = 0 && checkShrink v)
    //        elif value.Minute <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Kind = DateTimeKind.Unspecified && v.Millisecond = 0 && v.Second = 0 && v.Minute = 0 && checkShrink v)
    //        elif value.Hour <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Kind = DateTimeKind.Unspecified && v.Millisecond = 0 && v.Second = 0 && v.Minute = 0 && v.Hour = 0 && checkShrink v)
    //        else
    //            Seq.isEmpty shrinks
    //    checkShrink value

    //[<Fact>]
    //let ``TimeSpan``() =
    //    generate<TimeSpan> |> sample 10 |> ignore

    //[<Property>]
    //let ``TimeSpan shrinks`` (value: TimeSpan) =
    //    let rec checkShrink (value:TimeSpan) =
    //        let shrinks = shrink value
    //        if value.Days <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Days = 0 && checkShrink v)
    //        elif value.Hours <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Days = 0 && v.Hours = 0 && checkShrink v)
    //        elif value.Minutes <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Days = 0 && v.Hours = 0 && v.Minutes = 0 && checkShrink v)
    //        elif value.Seconds <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Days = 0 && v.Hours = 0 && v.Minutes = 0 && v.Seconds = 0 && checkShrink v)
    //        elif value.Milliseconds <> 0 then
    //            shrinks |> Seq.forall (fun v -> v.Ticks = 0L && checkShrink v)
    //        else
    //            Seq.isEmpty shrinks
    //    checkShrink value

    //[<Fact>]
    //let DateTimeOffset() =
    //    generate<DateTimeOffset> |> sample 10 |> ignore

    //[<Property>]
    //let ``DateTimeOffset shrinks`` (t: DateTimeOffset) =
    //    shrink t
    //    |> Seq.forall (fun v -> ((v.Offset.Hours = 0 || v.Offset.Minutes = 0) && v.DateTime = t.DateTime)
    //                            || (v.Offset.Hours = 0 && v.Offset.Minutes = 0 
    //                                    && (v.Millisecond = 0 // TODO: all cases below are redundant
    //                                        || (v.Millisecond = 0 && v.Second = 0)
    //                                        || (v.Millisecond = 0 && v.Second = 0 && v.Minute = 0) 
    //                                        || (v.Millisecond = 0 && v.Second = 0 && v.Minute = 0 && v.Hour = 0))))

    //[<Fact>]
    //let KeyValuePair () =
    //    generate<KeyValuePair<int,int>> |> sample 10 |> ignore

    //[<Property>]
    //let ``KeyValuePair shrinks`` (value: KeyValuePair<int, int>) =
    //    shrink value 
    //    |> Seq.forall (fun (KeyValue(k,v)) -> shrink value.Key |> Seq.exists ((=) k) || shrink value.Value |> Seq.exists ((=) v))
    //    |> assertTrue

    [<Fact>]
    let ``Array shrinks to shorter array or smaller elements``() =
        assertTrue ( shrink<int[]> |> fun (value, shrinks) ->
                shrinks |> Seq.forall (fun v -> v.Length < value.Length || Array.exists2 (fun e1 e2 -> abs e1 <= abs e2) v value) )
        
    //[<Property>]
    //let ``Array2D shrinks to smaller array or smaller elements`` (value:int[,]) =
    //    let existsSmallerElement v = 
    //        let result = ref false
    //        Array2D.iteri (fun i j elem -> result := (!result || abs elem <= abs value.[i,j])) v
    //        !result
    //    shrink value 
    //    |> Seq.forall (fun v -> 
    //            Array2D.length1 v < Array2D.length1 value
    //            || Array2D.length2 v < Array2D.length2 value
    //            || existsSmallerElement v)
    //    |> assertTrue
                
    [<Property>]
    let ``NonNegativeInt generates non negative ints`` (NonNegativeInt value) =
        value >= 0
        |> assertTrue
        
    [<Fact>]
    let ``NonNegativeInt shrinks non negative ints`` () =
        shrink<NonNegativeInt> |> snd
        |> Seq.forall (fun (NonNegativeInt v) -> v >= 0)
        |> assertTrue

    [<Property>]
    let ``PositiveInt generates positive ints`` (PositiveInt value) =
        value > 0
        |> assertTrue
       
    [<Fact>]
    let ``PositiveInt shrinks positive ints`` () =
        shrink<PositiveInt> |> snd
        |> Seq.forall (fun (PositiveInt v) -> v > 0)
        |> assertTrue

    [<Property>]
    let ``NegativeInt generates negative ints`` (NegativeInt value) =
        value < 0
        |> assertTrue

    [<Fact>]
    let ``NegativeInt shrinks negative ints`` () =
        let shrinks = shrink<NegativeInt> |> snd
        test <@ Seq.forall (fun (NegativeInt v) -> v < 0) shrinks @>
    
    //[<Property>]
    //let ``Interval after shrinking is smaller than origin`` (i : Interval) =
    //    let shrunk = shrink i
    //    let isWithoutDuplicates = (shrunk |> Seq.distinct |> Seq.length) = (Seq.length shrunk)
    //    let isSmaller = 
    //        shrunk
    //        |> Seq.map (fun i -> match i with Interval (start, end') -> (start, end'))
    //        |> Seq.forall (fun (start, end') -> (start <= i.Left) && (end' - start <= i.Right - i.Left))
    //    (isSmaller && isWithoutDuplicates)

    type TestEnum =
        | A = 0
        | B = 3
        | C = 4

    [<Property>]
    let Enum  (value:TestEnum) =
        List.exists (fun e -> e = int value) [0;3;4]
        |> assertTrue

    [<Flags>]
    type ByteFlags = A = 1uy | B = 2uy | C = 4uy

    [<Property>]
    let ``Can create unsigned byte flags enumeration`` (value : ByteFlags) =
        List.exists (fun e -> e = byte value) [0uy..7uy]
        |> assertTrue

    [<Flags>]
    type SByteFlags = A = 1y | B = 2y | C = 4y

    [<Property>]
    let ``Can create signed byte flags enumeration`` (value : SByteFlags) =
        List.exists (fun e -> e = sbyte value) [0y..7y]
        |> assertTrue

    [<Flags>]
    type UShortFlags = A = 1us | B = 2us | C = 4us

    [<Property>]
    let ``Can create 16-bit unsigned integer flags enumeration``
        (value : UShortFlags) =
        
        List.exists (fun e -> e = uint16 value) [0us..7us]
        |> assertTrue

    [<Flags>]
    type ShortFlags = A = 1s | B = 2s | C = 4s

    [<Property>]
    let ``Can create 16-bit integer flags enumeration`` (value : ShortFlags) =
        List.exists (fun e -> e = int16 value) [0s..7s]
        |> assertTrue

    [<Flags>]
    type UIntFlags = A = 1u | B = 2u | C = 4u

    [<Property>]
    let ``Can create 32-bit unsigned integer flags enumeration``
        (value : UIntFlags) =
        
        List.exists (fun e -> e = uint32 value) [0u..7u]
        |> assertTrue

    [<Flags>]
    type IntFlags = A = 1 | B = 2 | C = 4

    [<Property>]
    let ``Can create 32-bit integer flags enumeration`` (value : IntFlags) =
        List.exists (fun e -> e = int value) [0..7]
        |> assertTrue

    [<Flags>]
    type ULongFlags = A = 1UL | B = 2UL | C = 4UL

    [<Property>]
    let ``Can create 64-bit unsigned integer flags enumeration``
        (value : ULongFlags) =
        
        List.exists (fun e -> e = uint64 value) [0UL..7UL]
        |> assertTrue

    [<Flags>]
    type LongFlags = A = 1L | B = 2L | C = 4L

    [<Property>]
    let ``Can create 64-bit integer flags enumeration`` (value : LongFlags) =
        List.exists (fun e -> e = int64 value) [0L..7L]
        |> assertTrue
    
    type SimpleEnum = A = 0 | B = 1 | C = 2
    type [<Flags>] FlagsEnum = None = 0 | A = 1 | B = 2 | C = 4 | All = 7
    type FlagsShrunkResult = {Original: FlagsEnum; Shrunk: seq<FlagsEnum>}
    [<Fact>]
    let ``Only enums marked as flags are shrunk`` () =
        let flags,shrunkFlags = shrink<FlagsEnum>
        let simple,shrunkSimple = shrink<SimpleEnum>
        if flags = FlagsEnum.None then
            (shrunkFlags |> Seq.isEmpty) &&
            (shrunkSimple |> Seq.isEmpty)
        else
            not (shrunkFlags |> Seq.isEmpty) &&
            (shrunkSimple |> Seq.isEmpty)

    [<Fact>]
    let ``FsList shrunk is at minimum n-1``() =
        let (value, shrinks) = shrink<list<int>>
        let result = value.Length = 0 || (shrinks |> Seq.forall (fun s -> s.Length = value.Length || s.Length = value.Length - 1))
        assertTrue result
        

    //[<Fact>]
    //let ``Generic List``() =
    //    generate<List<int>> |> sample 10 |> ignore

    //[<Fact>]
    //let ``Generic IList``() =
    //    generate<IList<int>> |> sample 10 |> ignore

    //[<Property>]
    //let ``Generic IList shrinks`` (value: int IList) =
    //    shrink value 
    //    |> Seq.forall (fun l -> l.Count <= value.Count)
    //    |> assertTrue

    //[<Fact>]
    //let ``Generic ICollection``() =
    //    generate<ICollection<int>> |> sample 10 |> ignore

    //[<Property>]
    //let ``Generic ICollection shrinks`` (value: int ICollection) =
    //    shrink value 
    //    |> Seq.forall (fun l -> l.Count <= value.Count)
    //    |> assertTrue

    //[<Fact>]
    //let ``Generic Dictionary``() =
    //    generate<Dictionary<int, char>> |> sample 10 |> ignore

    //[<Fact>]
    //let ``Generic Dictionary with string key``() =
    //    generate<Dictionary<string, char>> |> sample 10 |> ignore

    //[<Property>]
    //let ``Generic Dictionary shrinks`` (value: Dictionary<int, string>) =
    //    shrink value 
    //    |> Seq.forall (fun l -> l.Count < value.Count)
    //    |> assertTrue

    //[<Fact>]
    //let ``Generic IDictionary``() =
    //    generate<IDictionary<int, char>> |> sample 10 |> ignore

    //[<Fact>]
    //let ``Map with string key``() =
    //    generate<Map<string, char>> |> sample 10 |> Seq.exists (fun x -> not x.IsEmpty)
    //    |> assertTrue

    //[<Property>]
    //let Decimal (size : PositiveInt) =
    //    generate<decimal> 
    //    |> Gen.sampleWithSize size.Get 10 
    //    |> Array.forall (fun d -> abs d < decimal size.Get)
    //    |> assertTrue

    //[<Property>]
    //let ``Decimal shrinks`` (value: decimal) =
    //    shrink<decimal> value 
    //    |> Seq.forall (fun shrunkv -> shrunkv = 0m || shrunkv <= abs value)
    //    |> assertTrue

    //[<Fact>]
    //let DoNotSizeDecimal() =
    //    generate<DoNotSize<decimal>> |> sample 10 |> ignore
    
    //[<Fact>]
    //let Complex() =
    //    generate<Numerics.Complex> |> sample 10

    //[<Property>]
    //let ``Complex shrinking produce values without duplicates and at least one without imaginary part`` (c : Numerics.Complex) =
    //    let shrunk = shrink c
    //    let isDistinct = (shrunk |> Seq.distinct |> Seq.length) = (Seq.length shrunk)
    //    if c.Imaginary <> 0.0 then
    //        isDistinct && Seq.exists (fun (sh : Numerics.Complex) -> sh.Imaginary = 0.0) shrunk
    //    else
    //        isDistinct

    //[<Fact>]
    //let Culture() =
    //    generate<CultureInfo> |> sample 10 |> ignore

//commented out as it keeps failing on AppVeyor with: ca-ES-valencia, pretty hard to repro.
//    [<Property>]
//    let ``Culture shrinks`` (value: CultureInfo) =
//        shrink<CultureInfo> value
//        |> Seq.forall (fun c -> c.IsNeutralCulture || c = CultureInfo.InvariantCulture)

    //[<Fact>]
    //let Guid () =
    //    generate<Guid> |> sample 10 |> ignore

    //[<Fact>]
    //let ConsoleKeyInfo () =
    //    generate<ConsoleKeyInfo> |> sample 10 |> ignore

    //[<Property>]
    //let ``ConsoleKeyInfo shrinker produce distinct sequence without origin`` (cki : ConsoleKeyInfo) =
    //    let shrunk = shrink cki
    //    let isDistinct = (shrunk |> Seq.distinct |> Seq.length) = (Seq.length shrunk)
    //    let hasOrigin = shrunk |> Seq.contains cki
    //    (isDistinct && not hasOrigin)

    //[<Fact>]
    //let IPAddress () =
    //    generate<IPAddress> |> sample 10 |> ignore

    //[<Property>]
    //let ``IPAddress shrinks`` (value: IPAddress) =
    //    let bytesSum (x: IPAddress) = x.GetAddressBytes() |> Array.sumBy int

    //    shrink value
    //    |> Seq.forall (fun shrunkv -> bytesSum shrunkv = 0 || bytesSum shrunkv < bytesSum value)
    //    |> assertTrue

    //[<Property>]
    //let ``IPv4Address is only IPv4`` (IPv4Address address) =
    //    address.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork
    //    |> assertTrue

    //[<Property>]
    //let ``IPv6Address is only IPv6`` (IPv6Address address) =
    //    address.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6

    //[<Property>]
    //let ``HostName is useful in an Uri`` (HostName host) =
    //    Uri.TryCreate (sprintf "http://%s" host, UriKind.Absolute) |> fst
    //    |> assertTrue

    //[<Property>]
    //let ``HostName correctly turns to string`` (HostName expected as value) =
    //    expected = string value

    //[<Fact>]
    //let MailAddress () =
    //    generate<MailAddress> |> sample 10 |> ignore

    //[<Property>]
    //let ``MailAddress shrinks`` (value: MailAddress) =
    //    shrink value
    //    |> Seq.forall (fun shrunkv -> shrunkv.ToString().Length < value.ToString().Length || shrunkv.Host <> value.Host)

    [<Fact>]
    let Bigint () =
        generate<bigint> |> sample 10 |> ignore
        shrink<bigint> |> fun (value,shrinks) -> shrinks |> Seq.forall (fun shrunkv -> shrunkv <= abs value) |> assertTrue

    type Empty() = class end

    [<Fact>]
    let ``Derive generator for concrete class with one constructor with no parameters``() =
        generate<Empty> |> sample 10 |> ignore

    type IntWrapper(a:int) = class end

    [<Fact>]
    let ``Derive generator for concrete class with one constructor with one parameter``() =
        generate<IntWrapper> |> sample 10 |> ignore

    type FakeRecord(a: int, b: string) =
        member __.A = a
        member __.B = b

    [<Fact>]
    let ``Derive generator for concrete class with one constructor with two parameters``() =
        generate<FakeRecord> |> sample 10 |> ignore

    [<Struct>]
    type StructRecord(a: int, b: string) =
        member __.A = a
        member __.B = b

    [<Fact>]
    let ``Derive generator for struct with one constructor with two parameters``() =
        generate<StructRecord> |> sample 10 |> ignore

    type FakeDto() =
        member val A = Unchecked.defaultof<string> with get, set
        member val B = Unchecked.defaultof<int> with get, set
        member val C = Unchecked.defaultof<System.Nullable<int>> with get, set
        member val D = Unchecked.defaultof<ResizeArray<string>> with get, set

    [<Fact>]
    let ``Derive generator for concrete DTO class with writable properties``() =
        generate<FakeDto> |> sample 10 |> ignore

    [<Fact>]
    let ``Derive generator for concrete DTO class shrinks`` () =
        let value,shrunk = shrink<FakeDto>
        // check that A gets smaller (length-wise)
        shrunk
        |> Seq.forall (fun shrunkv -> (isNull shrunkv.A) || shrunkv.A.Length = 0 || shrunkv.A.Length <= value.A.Length)
        &&
        // check that B gets smaller (in absolute value)
        shrunk
        |> Seq.forall (fun shrunkv -> shrunkv.B = 0 || shrunkv.B <= abs value.B)
        &&
        // check that C gets smaller (in absolute value, or to null)
        shrunk
        |> Seq.forall (fun shrunkv ->
                        if value.C.HasValue then
                            (not shrunkv.C.HasValue)
                            || shrunkv.C.Value = 0
                            || shrunkv.C.Value <= abs value.C.Value
                        else
                            not shrunkv.C.HasValue)
        &&
        // check that D gets smaller (length-wise)
        shrunk
        |> Seq.forall (fun shrunkv -> shrunkv.D.Count = 0 || shrunkv.D.Count <= value.D.Count)

    type PrivateRecord = private { A: int; B: string }

    [<Fact>]
    let ``Derive generator for private two value record``() =
        generate<PrivateRecord> |> sample 10 |> ignore

    type PrivateUnion = private | Case1 | Case2 of string

    [<Fact>]
    let ``Derive generator for private two case union``() =
        generate<PrivateUnion> |> sample 10 |> ignore

    [<Fact>]
    let ``should not crash on isCSharpDto issue #545``() =
        try
            Check.QuickThrowOnFailure <| fun (u: UriBuilder) -> ()
            failwith "Test should have failed because UriBuilder can not be generated"
        with exn as e ->
            test <@ e.InnerException.Message.Contains("is not handled automatically by FsCheck") @>

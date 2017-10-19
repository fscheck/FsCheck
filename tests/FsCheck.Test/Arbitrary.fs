
namespace FsCheck.Test

module Arbitrary =

    open Xunit    
    open FsCheck
    open FsCheck.Xunit
    open System
    open System.Globalization
    open System.Collections.Generic
#if !NETSTANDARD1_6
    open System.Net
    open System.Net.Mail
#endif
    open Helpers
    open Arb
    open Swensen.Unquote
    
    [<Property>]
    let Unit() = 
        assertTrue ( generate<unit> |> sample 10 |> Seq.forall ((=) ()) )
        assertTrue ( shrink<unit>() |> Seq.isEmpty )
    
    [<Property>]
    let Boolean (b:bool) =
        assertTrue ( generate<bool> |> sample 10 |> Seq.forall (fun _ -> true) )
        assertTrue ( shrink<bool> b |> Seq.isEmpty )
    
    [<Property>]
    let Int16 (NonNegativeInt size) (v:int16) =
        assertTrue ( generate<int16> |> Gen.resize size |> sample 10 |> Seq.forall (fun v -> -size <= int v && int v <= size) )
        assertTrue ( shrink<int16> v |> Seq.forall (fun shrunkv -> shrunkv <= abs v) )

    [<Property>]
    let DoNotSizeInt16 (DoNotSize v as doNotSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue ( generate<DoNotSize<int16>> |> Gen.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0s) )
        assertTrue ( shrink<DoNotSize<int16>> doNotSizeV |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= abs v) )

    [<Property>]
    let UInt16 (NonNegativeInt size) (v:uint16) =
        assertTrue (  generate<uint16> |> Gen.resize size |> sample 10 |> Seq.forall (fun v -> int v <= size) )
        assertTrue (  shrink<uint16> v |> Seq.forall (fun shrunkv -> shrunkv <= v) )

    [<Property>]
    let DoNotSizeUInt16 (DoNotSize v as doNotSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue (  generate<DoNotSize<uint16>> |> Gen.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0us) )
        assertTrue (  shrink<DoNotSize<uint16>> doNotSizeV |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= v) )

    [<Property>]
    let Int32 (NonNegativeInt size) (v:int) =
        assertTrue (  generate<int> |> Gen.resize size |> sample 10 |> Seq.forall (fun v -> -size <= v && v <= size) )
        assertTrue (  shrink<int> v |> Seq.forall (fun shrunkv -> shrunkv <= abs v) )

    [<Property>]
    let DoNotSizeInt32 (DoNotSize v as doNotSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue (  generate<DoNotSize<int>> |> Gen.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0) )
        assertTrue (  shrink<DoNotSize<int>> doNotSizeV |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= abs v) )

    [<Property>]
    let UInt32 (NonNegativeInt size) (v:uint32) =
        assertTrue ( generate<uint32> |> Gen.resize size |> sample 10 |> Seq.forall (fun v -> int v <= size) )
        assertTrue ( shrink<uint32> v |> Seq.forall (fun shrunkv -> shrunkv <= v) )

    [<Property>]
    let DoNotSizeUInt32 (DoNotSize v as doNotSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue (  generate<DoNotSize<uint32>> |> Gen.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0u) )
        assertTrue (  shrink<DoNotSize<uint32>> doNotSizeV |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= v) )

    [<Property>]
    let Int64 (NonNegativeInt size) (value: int64) = 
        assertTrue ( generate<int64> |> Gen.resize size |> sample 10 |> Seq.forall (fun v -> -(int64 size) <= v && v <= int64 size) )
        assertTrue ( shrink<int64> value |> Seq.forall (fun shrunkv -> (int shrunkv) <= abs (int value)) )
                
    [<Property>]
    let DoNotSizeInt64 (DoNotSize v as doNotSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue ( generate<DoNotSize<int64>> |> Gen.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0L) )
        assertTrue ( shrink<DoNotSize<int64>> doNotSizeV |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= abs v) )

    [<Property>]
    let UInt64 (NonNegativeInt size) (v:uint64) =
        assertTrue ( generate<uint64> |> Gen.resize size |> sample 10 |> Seq.forall (fun v -> int v <= size) )
        assertTrue ( shrink<uint64> v |> Seq.forall (fun shrunkv -> shrunkv <= v) )

    [<Property>]
    let DoNotSizeUInt64 (DoNotSize v as doNotSizeV) =
        //could theoretically go wrong, if all the values do happen to be zero.
        assertTrue ( generate<DoNotSize<uint64>> |> Gen.resize 0 |> sample 100 |> Seq.exists (fun (DoNotSize v) -> v <> 0UL) )
        assertTrue ( shrink<DoNotSize<uint64>> doNotSizeV |> Seq.forall (fun (DoNotSize shrunkv) -> shrunkv <= v) )

    [<Property>]
    let Double (NonNegativeInt size) (value:float) =
        assertTrue ( generate<float> |> Gen.resize size |> sample 10
                |> Seq.forall (fun v -> 
                    (-2.0 * float size <= v && v <= 2.0 * float size )
                    || Double.IsNaN(v) || Double.IsInfinity(v)
                    || v = Double.Epsilon || v = Double.MaxValue || v = Double.MinValue) )
        assertTrue ( shrink<float> value 
                |> Seq.forall (fun shrunkv -> shrunkv = 0.0 || shrunkv <= abs value) )
        
    [<Property>]
    let Single (NonNegativeInt size) (value:float32) =
        assertTrue ( generate<float32> |> Gen.resize size |> sample 10
                |> Seq.forall (fun v -> 
                    (-2.0f * float32 size <= v && v <= 2.0f * float32 size )
                    || Single.IsNaN(v) || Single.IsInfinity(v)
                    || v = Single.Epsilon || v = Single.MaxValue || v = Single.MinValue) )
        assertTrue ( shrink<float32> value 
                |> Seq.forall (fun shrunkv -> shrunkv = 0.0f || shrunkv <= abs value) )

    [<Property>]
    let Byte (value:byte) =
        assertTrue ( generate<byte> |> sample 10 |> Seq.forall (fun _ -> true) ) //just check that we can generate bytes
        assertTrue ( shrink<byte> value |> Seq.forall (fun shrunkv -> (int shrunkv) <= abs (int value)) )
        
    [<Property>]
    let SByte (value:sbyte) =
        assertTrue ( generate<sbyte> |> sample 10 |> Seq.forall (fun _ -> true) ) //just check that we can generate sbytes
        assertTrue ( shrink<sbyte> value |> Seq.forall (fun shrunkv -> int shrunkv <= abs (int value) ) )

    [<Property>]
    let Char (value:char) =
        assertTrue ( generate<char> |> sample 10 |> Seq.forall (fun v -> v >= Char.MinValue && (int v) <= 127) )
        assertTrue ( shrink<char> value |> Seq.forall (fun shrunkv -> isIn  ['a';'b';'c'] shrunkv ) )

    [<Property>]
    let String (value:string) =
        assertTrue ( generate<string> |> sample 10 |> Seq.forall (fun _ -> true) )
            //or the lenght of the string is shorter, or one of its values have been shrunk
        assertTrue ( shrink<string> value |> Seq.forall (fun s -> s = null || String.length s < String.length value || (String.exists (isIn ['a';'b';'c']) s))  )

    [<Property>]
    let ``Non-empty string`` (NonEmptyString v) = 
        not (System.String.IsNullOrEmpty v)
        |> assertTrue
      
    [<Property>]
    let ``2-Tuple``((valuei:int,valuec:char) as value) =
        assertTrue ( generate<int*char> |> sample 10 |> Seq.forall (fun _ -> true) )
            //or the first value is shrunk, or the second
        assertTrue ( shrink value |> Seq.forall (fun (i,c) -> shrink valuei |> Seq.exists ((=) i) || shrink valuec |> Seq.exists ((=) c)) )
    
    [<Property>]
    let ``3-Tuple``((valuei:int,valuec:char,valueb:bool) as value) =
        assertTrue ( generate<int*char*bool> |> sample 10 |> Seq.forall (fun _ -> true) )
            //or the first value is shrunk, or the second, or the third
        assertTrue ( shrink value |> Seq.forall (fun (i,c,b) -> shrink valuei |> Seq.exists ((=) i) 
                                                                || shrink valuec |> Seq.exists ((=) c)
                                                                || shrink valueb |> Seq.exists ((=) b)) )
     
    [<Property>]
    let Option (value:option<int>) =
        assertTrue ( generate<option<int>> |> sample 10 |> Seq.forall (fun _ -> true) )
        assertTrue ( shrink value 
                |> (fun shrinks -> match value with 
                                   | None -> shrinks = Seq.empty 
                                   | Some v ->  Seq.forall2 (=) shrinks (seq { yield None; for x' in shrink v -> Some x' }) ) )

    [<Property>]
    let NonNull (value:NonNull<string>) = 
        assertTrue ( generate<NonNull<string>> |> sample 10 |> Seq.forall (fun (NonNull x) -> x <> null) )
        assertTrue ( Seq.forall (fun (NonNull x) -> x <> null) (shrink value) )

    [<Property>]
    let Nullable (value:Nullable<int>) =
        generate<Nullable<int>> |> sample 10 |> ignore
        let shrinks = shrink value 
        if value.HasValue then 
            assertTrue (Seq.forall2 (=) shrinks (seq { yield Nullable(); for x' in shrink value.Value -> Nullable x' }))
        else 
            assertTrue (shrinks = Seq.empty)

    let testFunction (f: _ -> _) (vs: _ list) =
        let tabledF = Function<_,_>.From f
        assertTrue (List.map tabledF.Value vs = List.map f vs)
        assertTrue (List.forall (fun v -> List.tryFind (fst >> (=) v) tabledF.Table = Some (v,f v)) vs)
        
    [<Property>]
    let Function (f:int->int) (vs:list<int>) =
        testFunction f vs
    
    [<Property>]
    //checks that a generated function is pure by applying it twice to the same values and checking that the results are the same.
    let FunctionIsPure (f:int->char->bool) (vs:list<int*char>) =
        assertTrue <| List.forall2 (=)
            (List.map (fun (a,b) -> f a b) vs)
            (List.map (fun (a,b) -> f a b) vs)

    [<Property>]
    let ``Fun pattern works``(Fun (f:bool->bool)) =
        let _ = f true
        let _ = f false
        ()

    [<Property>]
    let ``ThrowingFunction throws exceptions from list`` (vs:list<int>) =
        let exceptions : Exception list = 
            [ NullReferenceException("exc1")
              ArgumentNullException("exc2") ]
        let catch (ThrowingFunction f) v = 
            try
                f v |> ignore
            with 
            | :? NullReferenceException as e -> assertTrue (e.Message = "exc1")
            | :? ArgumentNullException as e -> assertTrue (e.ParamName = "exc2")

        Prop.forAll (Arb.Default.ThrowingFunction<int,int>(exceptions)) (fun f -> vs |> Seq.iter (catch f))

    [<Property>]
    let ``ThrowingFunction throws exceptions`` (vs:list<int>) (ThrowingFunction f) =
        let catch f v = 
            try
                let _ : int = f v
                ()
            with 
            | _ -> ()

        vs |> Seq.iter (catch f)

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
            | null | :? char | :? bool | :? string -> true
            | _ -> false
        let goodShrinks (o:obj) shrinks = Seq.forall2 (=) (shrink (unbox o)) (shrinks |> Seq.map unbox)
        assertTrue (goodObject o)
        assertTrue (shrink o |> goodShrinks o)
            
    [<Property>]
    let ``DateTime generates year between 1900 and 2100``(value:DateTime) =
        assertTrue (1900 <= value.Year)
        assertTrue (value.Year <= 2100)

    [<Property>]
    let ``DateTime shrinks seconds, minutes and hours`` (value:DateTime) =
        shrink value
        |> Seq.forall (fun v -> v.Second = 0 
                                 || (v.Second = 0 && v.Minute = 0) 
                                 || (v.Second = 0 && v.Minute = 0 && v.Hour = 0) )
        |> assertTrue

    [<Fact>]
    let ``TimeSpan``() =
        generate<TimeSpan> |> sample 10 |> ignore

    [<Property>]
    let ``TimeSpan shrinks`` (value: TimeSpan) =
        shrink value
        |> Seq.forall (fun v -> v.Days = 0
                                || (v.Days = 0 && v.Hours = 0)
                                || (v.Days = 0 && v.Hours = 0 && v.Minutes = 0)
                                || (v.Days = 0 && v.Hours = 0 && v.Minutes = 0 && v.Seconds = 0)
                                || (v.Days = 0 && v.Hours = 0 && v.Minutes = 0 && v.Seconds = 0 || v.Milliseconds = 0))
        |> assertTrue

    [<Fact>]
    let DateTimeOffset() =
        generate<DateTimeOffset> |> sample 10 |> ignore

    [<Property>]
    let ``DateTimeOffset shrinks`` (t: DateTimeOffset) =
        shrink t
        |> Seq.forall (fun v -> ((v.Offset.Hours = 0 || v.Offset.Minutes = 0) && v.DateTime = t.DateTime)
                                || (v.Offset.Hours = 0 && v.Offset.Minutes = 0 
                                        && (v.Second = 0 
                                            || (v.Second = 0 && v.Minute = 0) 
                                            || (v.Second = 0 && v.Minute = 0 && v.Hour = 0))))
        |> assertTrue

    [<Fact>]
    let KeyValuePair () =
        generate<KeyValuePair<int,int>> |> sample 10 |> ignore

    [<Property>]
    let ``KeyValuePair shrinks`` (value: KeyValuePair<int, int>) =
        shrink value 
        |> Seq.forall (fun (KeyValue(k,v)) -> shrink value.Key |> Seq.exists ((=) k) || shrink value.Value |> Seq.exists ((=) v))
        |> assertTrue

    [<Property>]
    let ``Array shrinks to shorter array or smaller elements`` (value:int[]) =
        assertTrue ( shrink value 
                |> Seq.forall (fun v -> v.Length < value.Length || Array.exists2 (fun e1 e2 -> abs e1 <= abs e2) v value) )
        
    [<Property>]
    let ``Array2D shrinks to smaller array or smaller elements`` (value:int[,]) =
        let existsSmallerElement v = 
            let result = ref false
            Array2D.iteri (fun i j elem -> result := (!result || abs elem <= abs value.[i,j])) v
            !result
        shrink value 
        |> Seq.forall (fun v -> 
                Array2D.length1 v < Array2D.length1 value
                || Array2D.length2 v < Array2D.length2 value
                || existsSmallerElement v)
        |> assertTrue
                
    [<Property>]
    let ``NonNegativeInt generates non negative ints`` (NonNegativeInt value) =
        value >= 0
        |> assertTrue
        
    [<Property>]
    let ``NonNegativeInt shrinks non negative ints`` (value:NonNegativeInt) =
        shrink value |> Seq.forall (fun (NonNegativeInt v) -> v >= 0)
        |> assertTrue

    [<Property>]
    let ``PositiveInt generates positive ints`` (PositiveInt value) =
        value > 0
        |> assertTrue
       
    [<Property>]
    let ``PositiveInt shrinks positive ints`` (value:PositiveInt ) =
        shrink value |> Seq.forall (fun (PositiveInt v) -> v > 0)
        |> assertTrue

    [<Property>]
    let ``NegativeInt generates negative ints`` (NegativeInt value) =
        value < 0
        |> assertTrue

    [<Property>]
    let ``NegativeInt shrinks negative ints`` (value:NegativeInt ) =
        shrink value |> Seq.forall (fun (NegativeInt v) -> v < 0)
        |> assertTrue

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

    [<Fact>]
    let ``FsList shrunk is at minimum n-1``() =
        let prop (l:int list) = 
            let shrunk = Arb.Default.FsList().Shrinker l |> List.ofSeq
            let result = l.Length = 0 || (shrunk |> Seq.forall (fun s -> s.Length = l.Length || s.Length = l.Length - 1))
            assertTrue result
        Check.QuickThrowOnFailure prop

    [<Fact>]
    let ``Generic List``() =
        generate<List<int>> |> sample 10 |> ignore

    [<Fact>]
    let ``Generic IList``() =
        generate<IList<int>> |> sample 10 |> ignore

    [<Property>]
    let ``Generic IList shrinks`` (value: int IList) =
        shrink value 
        |> Seq.forall (fun l -> l.Count <= value.Count)
        |> assertTrue

    [<Fact>]
    let ``Generic ICollection``() =
        generate<ICollection<int>> |> sample 10 |> ignore

    [<Property>]
    let ``Generic ICollection shrinks`` (value: int ICollection) =
        shrink value 
        |> Seq.forall (fun l -> l.Count <= value.Count)
        |> assertTrue

    [<Fact>]
    let ``Generic Dictionary``() =
        generate<Dictionary<int, char>> |> sample 10 |> ignore

    [<Fact>]
    let ``Generic Dictionary with string key``() =
        generate<Dictionary<string, char>> |> sample 10 |> ignore

    [<Property>]
    let ``Generic Dictionary shrinks`` (value: Dictionary<int, string>) =
        shrink value 
        |> Seq.forall (fun l -> l.Count < value.Count)
        |> assertTrue

    [<Fact>]
    let ``Generic IDictionary``() =
        generate<IDictionary<int, char>> |> sample 10 |> ignore

    [<Fact>]
    let ``Map with string key``() =
        generate<Map<string, char>> |> sample 10 |> Seq.exists (fun x -> not x.IsEmpty)
        |> assertTrue

    [<Fact>]
    let Decimal() =
        generate<decimal> |> sample 10 |> ignore

    [<Property>]
    let ``Decimal shrinks`` (value: decimal) =
        shrink<decimal> value 
        |> Seq.forall (fun shrunkv -> shrunkv = 0m || shrunkv <= abs value)
        |> assertTrue

    [<Fact>]
    let Culture() =
        generate<CultureInfo> |> sample 10 |> ignore

//commented out as it keeps failing on AppVeyor with: ca-ES-valencia, pretty hard to repro.
//    [<Property>]
//    let ``Culture shrinks`` (value: CultureInfo) =
//        shrink<CultureInfo> value
//        |> Seq.forall (fun c -> c.IsNeutralCulture || c = CultureInfo.InvariantCulture)

    [<Fact>]
    let Guid () =
        generate<Guid> |> sample 10 |> ignore
#if !NETSTANDARD1_6
    [<Fact>]
    let IPAddress () =
        generate<IPAddress> |> sample 10 |> ignore

    [<Property>]
    let ``IPAddress shrinks`` (value: IPAddress) =
        let bytesSum (x: IPAddress) = x.GetAddressBytes() |> Array.sumBy int

        shrink value
        |> Seq.forall (fun shrunkv -> bytesSum shrunkv = 0 || bytesSum shrunkv < bytesSum value)
        |> assertTrue

    [<Property>]
    let ``IPv4Address is only IPv4`` (IPv4Address address) =
        address.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork
        |> assertTrue

    [<Property>]
    let ``IPv6Address is only IPv6`` (IPv6Address address) =
        address.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6
#endif

    [<Property>]
    let ``HostName is useful in an Uri`` (HostName host) =
        Uri.TryCreate (sprintf "http://%s" host, UriKind.Absolute) |> fst
        |> assertTrue

    [<Property>]
    let ``HostName correctly turns to string`` (HostName expected as value) =
        expected = string value

#if !NETSTANDARD1_6
    [<Fact>]
    let MailAddress () =
        generate<MailAddress> |> sample 10 |> ignore

    [<Property>]
    let ``MailAddress shrinks`` (value: MailAddress) =
        shrink value
        |> Seq.forall (fun shrunkv -> shrunkv.ToString().Length < value.ToString().Length || shrunkv.Host <> value.Host)
#endif

    [<Property>]
    let Bigint (value:bigint) =
        generate<bigint> |> sample 10 |> ignore
        shrink<bigint> value |> Seq.forall (fun shrunkv -> shrunkv <= abs value) |> assertTrue

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

    type FakeDto() =
        member val A = Unchecked.defaultof<string> with get, set
        member val B = Unchecked.defaultof<int> with get, set
        member val C = Unchecked.defaultof<System.Nullable<int>> with get, set
        member val D = Unchecked.defaultof<ResizeArray<string>> with get, set

    [<Fact>]
    let ``Derive generator for concrete DTO class with writable properties``() =
        generate<FakeDto> |> sample 10 |> ignore


    type PrivateRecord = private { a: int; b: string }

    [<Fact>]
    let ``Derive generator for private two value record``() =
        generate<PrivateRecord> |> sample 10 |> ignore

    type PrivateUnion = private | Case1 | Case2 of string

    [<Fact>]
    let ``Derive generator for private two case union``() =
        generate<PrivateUnion> |> sample 10 |> ignore

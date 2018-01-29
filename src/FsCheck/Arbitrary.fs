namespace FsCheck

// Disable warnings about calling certain FsCheck functions from F#.
// Using them internally within FsCheck is important for performance reasons.
#nowarn "10001"

open System

#if NETSTANDARD1_6
#else
open System.Net
open System.Net.Mail
#endif

///Represents an int < 0
type NegativeInt = NegativeInt of int with
    member x.Get = match x with NegativeInt r -> r
    override x.ToString() = x.Get.ToString()
    static member op_Explicit(NegativeInt i) = i

///Represents an int >= 0
type NonNegativeInt = NonNegativeInt of int with
    member x.Get = match x with NonNegativeInt r -> r
    override x.ToString() = x.Get.ToString()
    static member op_Explicit(NonNegativeInt i) = i

///Represents an int > 0
type PositiveInt = PositiveInt of int with
    member x.Get = match x with PositiveInt r -> r
    override x.ToString() = x.Get.ToString()
    static member op_Explicit(PositiveInt i) = i

///Represents an int <> 0
type NonZeroInt = NonZeroInt of int with
    member x.Get = match x with NonZeroInt r -> r
    override x.ToString() = x.Get.ToString()
    static member op_Explicit(NonZeroInt i) = i

///Represents a float that is not NaN or Infinity.
type NormalFloat = NormalFloat of float with
    member x.Get = match x with NormalFloat f -> f
    override x.ToString() = x.Get.ToString()
    static member op_Explicit(NormalFloat f) = f

///Represents a string that is not null or empty, and does not contain any null characters ('\000')
type NonEmptyString = NonEmptyString of string with
    member x.Get = match x with NonEmptyString r -> r
    override x.ToString() = x.Get

///Represents a string that does not contain null characters ('\000')
type StringNoNulls = StringNoNulls of string with
    member x.Get = match x with StringNoNulls r -> r
    override x.ToString() = x.Get

///Represents an integer interval.
type Interval = Interval of int * int with
    member x.Left = match x with Interval (l,_) -> l
    member x.Right = match x with Interval (_,r) -> r

///Represents an int that can include int.MinValue and int.MaxValue.
type IntWithMinMax = IntWithMinMax of int with
    member x.Get = match x with IntWithMinMax r -> r
    override x.ToString() = x.Get.ToString()
    static member op_Explicit(IntWithMinMax i) = i

///Represents a non-empty Set.
type NonEmptySet<'a when 'a : comparison> = NonEmptySet of Set<'a> with
    member x.Get = match x with NonEmptySet r -> r
    
///Represents a non-empty array.
type NonEmptyArray<'a> = NonEmptyArray of 'a[] with
    member x.Get = match x with NonEmptyArray r -> r

///Represents an array whose length does not change when shrinking.
type FixedLengthArray<'a> = FixedLengthArray of 'a[] with
    member x.Get = match x with FixedLengthArray r -> r

///Wrap a type in NonNull to prevent null being generated for the wrapped type.
type NonNull<'a when 'a : null> = NonNull of 'a with
    member x.Get = match x with NonNull r -> r

type ThrowingFunction<'a,'b> = ThrowingFunction of ('a -> 'b) with
    member x.Get = match x with ThrowingFunction f -> f

///A function (F# function) that can be displayed and shrunk.
[<StructuredFormatDisplay("{StructuredDisplayAsTable}")>]
[<NoEquality;NoComparison>]
type Function<'a,'b when 'a : comparison> = F of ref<list<('a*'b)>> * ('a ->'b) with
    member x.Value = match x with F (_,f) -> f
    member x.Table = match x with F (table,_) -> !table
    member x.StructuredDisplayAsTable =
        x.ToString()
    override x.ToString() =
        let layoutTuple (x,y) = sprintf "%A->%A" x y
        x.Table 
        |> Seq.distinctBy fst 
        |> Seq.sortBy fst 
        |> Seq.map layoutTuple 
        |> String.concat "; "
        |> sprintf "{ %s }"
    static member From f = 
        let table = ref []
        F (table,fun x -> let y = f x in table := (x,y)::(!table); y)

///Use the generator for 'a, but don't shrink.
type DoNotShrink<'a> = DoNotShrink of 'a

///Whereas most types are restricted by a size that grows
///as the test gets further, by applying this type the underlying
///type will ignore this size and always generate from the full range.
///Note that this only makes a difference for types that have a range -
///currently Int16, Int32, Int64 have DoNotSize Arbitrary instances.
///This is typically (and at least currently) only applicable for value types
///that are comparable, hence the type constraints.
type DoNotSize<'a when 'a : struct and 'a : comparison> = 
    DoNotSize of 'a with
    static member Unwrap(DoNotSize a) : 'a = a

#if NETSTANDARD1_6
#else
type IPv4Address = IPv4Address of IPAddress
type IPv6Address = IPv6Address of IPAddress
#endif

type HostName = HostName of string with
    override x.ToString () = match x with HostName s -> s

[<AutoOpen>]
module ArbPatterns =
    let (|Fun|) (f:Function<'a,'b>) = f.Value

module Arb =

    open System.Globalization
    open System.Collections.Generic
    open System.Linq
    open TypeClass    
    open System.ComponentModel
    open System.Threading
    open Data

    [<AbstractClass;Sealed>]
    type Default = class end

    let internal defaultArbitrary = 
        let empty = TypeClass<Gen<obj>>.New()
        empty.Discover(onlyPublic=true,instancesType=typeof<Default>)

    let internal arbitrary = new ThreadLocal<TypeClass<Gen<obj>>>(fun () -> defaultArbitrary)

    ///Register the generators that are static members of the given type.
    [<CompiledName("Register")>]
    let registerByType t = 
        let newTypeClass = arbitrary.Value.Discover(onlyPublic=true,instancesType=t)
        let result = arbitrary.Value.Compare newTypeClass
        arbitrary.Value <- arbitrary.Value.Merge newTypeClass

    ///Register the generators that are static members of the type argument.
    [<CompiledName("Register")>]
    let register<'t>() = registerByType typeof<'t>

    ///Returns a Gen<'Value>.
    [<CompiledName("Generate")>]
    let generate<'Value> = arbitrary.Value.InstanceFor<'Value,Gen<'Value>>()


    let internal getGenerator t = arbitrary.Value.GetInstance t |> unbox<IGen> |> (fun arb -> arb.AsGenObject)
  
    ///A collection of default generators.
    type Default with
        static member private fraction (a:int) (b:int) (c:int) = 
            double a + double b / (abs (double c) + 1.0) 
        
        ///Generates (), of the unit type.
        static member Unit = Gen.constant ()

        ///Generates an arbitrary bool.
        static member Bool = Gen.elements [true; false]

        ///Generates an arbitrary byte.
        static member Byte = Gen.choose (0,255) |> Gen.map byte

        ///Generates an arbitrary signed byte.
        static member SByte = Gen.chooseAround 0 (-128,127) |> Gen.map sbyte

        ///Generate arbitrary int16 that is between -size and size.
        static member Int16 = Default.Int32 |> Gen.map int16

        ///Generate arbitrary int16 that is uniformly distributed in the whole range of int16 values.
        static member DoNotSizeInt16 = Gen.chooseAround 0 (int Int16.MinValue, int Int16.MaxValue) |> Gen.map (int16 >> DoNotSize)

        ///Generate arbitrary uint16 that is between 0 and size.
        static member UInt16 = Default.Int32 |> Gen.map uint16

        ///Generate arbitrary uint16 that is uniformly distributed in the whole range of uint16 values.
        static member DoNotSizeUInt16 = Gen.chooseAround 0 (0, int UInt16.MaxValue) |> Gen.map (uint16 >> DoNotSize)
            
        ///Generate arbitrary int32 that is between -size and size.
        static member Int32 = Gen.sized (fun n -> Gen.choose (-n,n))

        ///Generate arbitrary int32 that is unrestricted by size.
        static member DoNotSizeInt32 = Gen.choose64Around 0L (int64 Int32.MinValue, int64 Int32.MaxValue) |> Gen.map (int >> DoNotSize)

        ///Generate arbitrary uint32 that is between 0 and size.
        static member UInt32 = Default.Int32 |> Gen.map (abs >> uint32)

        ///Generate arbitrary uint32 that is unrestricted by size.
        static member DoNotSizeUInt32 = Gen.choose64 (0L, int64 UInt32.MaxValue) |> Gen.map (uint32 >> DoNotSize)

        ///Generate arbitrary int64 that is between -size and size.
        ///Note that since the size is an int32, this does not actually cover the full
        ///range of int64. See DoNotSize<int64> instead.
        static member Int64 = Default.Int32 |> Gen.map int64

        ///Generate arbitrary int64 that is unrestricted by size.
        static member DoNotSizeInt64 = Gen.choose64Around 0L (Int64.MinValue, Int64.MaxValue) |> Gen.map DoNotSize
        
        ///Generate arbitrary uint64 that is between 0 and size.
        static member UInt64 = Default.Int32 |> Gen.map (abs >> int64)
        
        ///Generate arbitrary uint64 that is unrestricted by size.
        static member DoNotSizeUInt64 = 
            Gen.choose64Around 0L (Int64.MinValue, Int64.MaxValue)
            |> Gen.map (fun v -> BitConverter.ToUInt64(BitConverter.GetBytes(v), 0) |> DoNotSize)

        static member internal ShrinkDouble fl =
            let (|<|) x y = abs x < abs y
            [|    if Double.IsInfinity fl || Double.IsNaN fl then 
                        yield 0.0
                    else
                        if fl < 0.0 then yield -fl
                        let truncated = truncate fl
                        if truncated |<| fl then yield truncated |]
            |> Seq.distinct

        /// Generates a "normal" 64 bit floats (without NaN, Infinity, Epsilon, MinValue, MaxValue)
        static member NormalFloat =
            Gen.map3 Default.fraction Default.Int32 Default.Int32 Default.Int32
            |> Gen.shrink Default.ShrinkDouble
            |> Gen.map NormalFloat

        ///Generates arbitrary 64 bit floats, NaN, NegativeInfinity, PositiveInfinity, 
        ///Maxvalue, MinValue, Epsilon included fairly frequently.
        static member Float = 
            Gen.frequency [ 3, Gen.map3 Default.fraction Default.Int32 Default.Int32 Default.Int32
                            1, Gen.elements [ Double.NaN; Double.NegativeInfinity; Double.PositiveInfinity
                                              Double.MaxValue; Double.MinValue; Double.Epsilon] ]
            |> Gen.shrink Default.ShrinkDouble

        static member internal ShrinkSingle fl =
            let (|<|) x y = abs x < abs y
            seq {   if Single.IsInfinity fl || Single.IsNaN fl then 
                        yield 0.0f
                    else
                        if fl < 0.0f then yield -fl
                        let truncated = truncate fl
                        if truncated |<| fl then yield truncated }
            |> Seq.distinct

        //static member NormalSingle =
        //    let fraction a b c = float32 (Default.fraction a b c)
        //    Gen.map3 fraction Default.Int32 Default.Int32 Default.Int32
        //    |> Gen.shrink Default.ShrinkSingle
        //    |> Gen.map NormalSingle

        ///Generate arbitrary 32 bit floats, NaN, NegativeInfinity, PositiveInfinity, Maxvalue, MinValue, Epsilon included fairly frequently.
        static member Float32 = 
            let fraction a b c = float32 (Default.fraction a b c)
            Gen.frequency   [ 3, Gen.map3 fraction Default.Int32 Default.Int32 Default.Int32
                              1, Gen.elements [ Single.NaN; Single.NegativeInfinity; Single.PositiveInfinity
                                                Single.MaxValue; Single.MinValue; Single.Epsilon] ]
            |> Gen.shrink Default.ShrinkSingle

        ///Generate arbitrary decimal.
        static member Decimal =
            let shrinkDecimal d =
                let (|<|) x y = abs x < abs y
                seq {
                    if d < 0m then yield -d
                    let truncated = truncate d
                    if truncated |<| d then yield truncated
                }
            Gen.map5 (fun lo mid hi isNegative scale -> Decimal(lo, mid, hi, isNegative, scale))
                     Default.Int32 Default.Int32 Default.Int32 Default.Bool (Gen.choose(0, 28) |> Gen.map byte)
            |> Gen.shrink shrinkDecimal
            
        ///Generates arbitrary chars, between ASCII codes Char.MinValue and 127.
        static member Char = 
            Gen.chooseAround (int 'a') (int Char.MinValue, 127) |> Gen.map char

        ///Generates arbitrary strings, which are lists of chars generated by Char.
        static member String =
            // TODO this should be enough also for shrinking IF:
            // - shrinking on Gen.arrayOf and Gen.listOf is implemented
            Gen.frequency [ 9, Gen.map (fun (chars:char[]) -> new String(chars)) (Gen.arrayOf Default.Char)
                            1, Gen.constant null]

        ///Generate an option value that is 'None' 1/8 of the time.
        static member Option() = 
            Gen.optionOf generate

        ///Generate underlying values that are not null.
        static member NonNull() =
            let inline notNull x = not (LanguagePrimitives.PhysicalEquality null x)
            generate |> Gen.where notNull |> Gen.map NonNull

        ///Generate a nullable value that is null 1/8 of the time.
        static member Nullable() = 
            Gen.frequency [(1, gen { return Nullable() }); (7, Gen.map Nullable generate)]

        ///Generate a list of values. The size of the list is between 0 and the test size + 1.
        static member FsList() = 
            Gen.listOf generate

        ///Generate an object - a boxed char, string or boolean value.
        static member Object =
            Gen.oneof [| Gen.map box <| generate<bool>; Gen.map box <| generate<char>; Gen.map box <| generate<string> |]

        ///Generate a rank 1 array.
        static member Array() =
            Gen.arrayOf generate

        ///Generate a rank 2, zero based array.
        static member Array2D() = Gen.array2DOf generate
            //let shrinkArray2D (arr:_[,]) =
            //    let removeRow r (arr:_[,]) =
            //        Array2D.init (Array2D.length1 arr-1) (Array2D.length2 arr) (fun i j -> if i < r then arr.[i,j] else arr.[i+1,j])
            //    let removeCol c (arr:_[,]) =
            //        Array2D.init (Array2D.length1 arr) (Array2D.length2 arr-1) (fun i j -> if j < c then arr.[i,j] else arr.[i,j+1])
            //    seq { for r in 1..Array2D.length1 arr do yield removeRow r arr
            //          for c in 1..Array2D.length2 arr do yield removeCol c arr
            //          for i in 0..Array2D.length1 arr-1 do //hopefully the matrix is shrunk considerably before we get here...
            //            for j in 0..Array2D.length2 arr-1 do
            //                for elem in shrink arr.[i,j] do
            //                    let shrunk = Array2D.copy arr
            //                    shrunk.[i,j] <- elem
            //                    yield shrunk
            //        }

         ///Generate a function value. Function values can be generated for types 'a->'b where 'b has an Arbitrary instance.
         ///There is no shrinking for function values.
        static member Arrow() = Gen.pureFunction generate

        ///Generate a F# function value. Function values can be generated for types 'a->'b where 'b has an Arbitrary instance.
         ///There is no shrinking for function values.
        [<CompiledName("FSharpFun")>]
        static member Fun() = Default.Arrow()

        ///Generate am F# function value that generates an instance of the function result type about half the time. The other 
        ///times it generates one of the given exceptions.
        [<CompiledName("ThrowingFSharpFun")>]
        static member ThrowingFunction(exceptions:Exception seq) = 
            let exc = exceptions |> Seq.toArray
            let throwExc = Gen.elements exc |> Gen.map raise
            Gen.pureFunction (Gen.frequency [(exc.Length, generate);(exc.Length, throwExc)])
            |> Gen.map ThrowingFunction


        ///Generate an F# function value that generates an instance of the function result type about half the time. The other 
        ///times it generates one of a list of common .NET exceptions, including Exception, ArgumentException, ArithmeticException,
        ///IOException, NotImplementedException, OUtOfMemoryException and others.
        [<CompiledName("ThrowingFSharpFun")>]
        static member ThrowingFunction() = 
            Default.ThrowingFunction [| Exception()

                                        ArgumentException()
                                        ArgumentNullException()
                                        ArgumentOutOfRangeException()
                                        
                                        ArithmeticException()
                                        DivideByZeroException()
                                        OverflowException()
                                        FormatException()
                                        IndexOutOfRangeException()
                                        InvalidCastException()
                                        InvalidOperationException()
                                        
                                        IO.IOException()
                                        IO.EndOfStreamException()
                                        IO.FileNotFoundException()

                                        NotImplementedException()
                                        NotSupportedException()

                                        NullReferenceException()
                                        OutOfMemoryException()
#if NETSTANDARD1_6
#else
                                        NotFiniteNumberException()
                                        StackOverflowException()
#endif
                                        IO.DirectoryNotFoundException()
                                        IO.FileLoadException()
                                        KeyNotFoundException()
                                        IO.PathTooLongException()
                                     |]

        /////Generate a Function value that can be printed and shrunk. Function values can be generated for types 'a->'b 
        /////where 'b has an Arbitrary instance.
        //static member Function() =
        //    Gen.map Function<'a,'b>.From generate
        //    // TODO integrate in Gen.pureFSharpFunc? so this is then pointless?
        //    let shrinker f = 
        //        let update x' y' f x = if x = x' then y' else f x
        //        seq { for (x,y) in f.Table do 
        //                for y' in shrink y do 
        //                    yield Function<'a,'b>.From (update x y' f.Value) }
            

        ///Generates a Func'1.
        static member SystemFunc() =
            Default.Fun()
            |> Gen.map (fun f -> Func<_>(f))

        ///Generates a Func'2.
        static member SystemFunc1() =
            Default.Fun()
            |> Gen.map (fun f -> Func<_,_>(f))

        ///Generates a Func'3.
        static member SystemFunc2() =
            Default.Fun()
            |> Gen.map (fun f -> Func<_,_,_>(f))

        ///Generates a Func'4.
        static member SystemFunc3() =
            Default.Fun()
            |> Gen.map (fun f -> Func<_,_,_,_>(f))

        ///Generates an Action'0
        static member SystemAction() =
            Default.Fun()
            |> Gen.map (fun f -> Action(f))

        ///Generates an Action'1
        static member SystemAction1() =
            Default.Fun()
            |> Gen.map (fun f -> Action<_>(f))

        ///Generates an Action'2
        static member SystemAction2() =
            Default.Fun()
            |> Gen.map (fun f -> Action<_,_>(f))

        ///Generates an Action'3
        static member SystemAction3() =
            Default.Fun()
            |> Gen.map (fun f -> Action<_,_,_>(f))

        ///Generates an arbitrary DateTime between 1900 and 2100. 
        ///A DateTime shrinks by removing its second, minute and hour components.
        static member DateTime = 
            gen { let! y,m = Gen.zip (Gen.choose(1900,2100)) (Gen.choose(1, 12))
                  return! (fun d (h,min,sec) -> DateTime(y, m, d, h, min, sec)) 
                            <!> Gen.choose(1, DateTime.DaysInMonth(y, m))
                            <*> Gen.zip3 (Gen.choose(0,23)) (Gen.choose(0,59)) (Gen.choose(0,59))
            }

        /////Generates an arbitrary TimeSpan. A TimeSpan is shrunk by removing days, hours, minutes, second and milliseconds.
        //static member TimeSpan() =
        //    let genTimeSpan = generate |> Gen.map (fun (DoNotSize ticks) -> TimeSpan ticks)
        //    let shrink (t: TimeSpan) = 
        //        if t.Days > 0 then
        //            seq { yield TimeSpan(0, t.Hours, t.Minutes, t.Seconds, t.Milliseconds) }
        //        elif t.Hours > 0 then
        //            seq { yield TimeSpan(0, 0, t.Minutes, t.Seconds, t.Milliseconds) }
        //        elif t.Minutes > 0 then
        //            seq { yield TimeSpan(0, 0, 0, t.Seconds, t.Milliseconds) }
        //        elif t.Seconds > 0 then
        //            seq { yield TimeSpan(0, 0, 0, 0, t.Milliseconds) }
        //        elif t.Milliseconds > 0 then
        //            seq { yield TimeSpan(0L) }
        //        else
        //            Seq.empty
        //    fromGenShrink (genTimeSpan, shrink)

        /////Generates an arbitrary DateTimeOffset between 1900 and 2100. 
        ///// A DateTimeOffset is shrunk first by shrinking its offset, then by removing its second, minute and hour components.
        //static member DateTimeOffset() =
        //    let genTimeZone = gen {
        //                        let! hours = Gen.choose(-14, 14)
        //                        let! minutes = 
        //                            if abs hours = 14 then 
        //                                Gen.constant 0
        //                            else 
        //                                Gen.choose(0, 59)
        //                        return TimeSpan(hours, minutes, 0) }
        //    let shrinkTimeZone (t: TimeSpan) =
        //        shrink t |> Seq.where (fun z -> z.Hours > 0 || z.Minutes > 0)
        //    let genDate = gen { 
        //                    let! t = generate<DateTime>
        //                    let! tz = genTimeZone
        //                    return DateTimeOffset(t, tz) }
        //    let shrink (d: DateTimeOffset) =
        //        seq {
        //            for ts in shrinkTimeZone d.Offset ->
        //                DateTimeOffset(d.DateTime, ts)
        //            if d.Offset <> TimeSpan.Zero then
        //                yield DateTimeOffset(d.DateTime, TimeSpan.Zero)
        //            for dt in shrink d.DateTime ->
        //                DateTimeOffset(dt, TimeSpan.Zero) }
        //    fromGenShrink (genDate, shrink)

        static member KeyValuePair() = Gen.zip generate generate |> Gen.map KeyValuePair<_,_>

        static member NegativeInt =
            Default.Int32
            |> Gen.map (fun x -> -abs x) 
            |> Gen.where (fun x -> x < 0)
            |> Gen.map NegativeInt

        static member NonNegativeInt =
           Default.Int32
           |> Gen.map abs 
           |> Gen.where (fun i -> i >= 0)
           |> Gen.map NonNegativeInt

        static member PositiveInt =
            Default.Int32
            |> Gen.map abs 
            |> Gen.where (fun i -> i > 0)
            |> Gen.map PositiveInt

        static member NonZeroInt =
           Default.Int32
            |> Gen.where ((<>) 0)
            |> Gen.map NonZeroInt

        static member IntWithMinMax =
            Gen.frequency [ 10 ,Default.Int32 
                            1,  Gen.elements [Int32.MaxValue; Int32.MinValue] ]
            |> Gen.map IntWithMinMax

        ///Generates an interval between two non-negative integers.
        static member Interval = 
            Gen.two Default.Int32 
            |> Gen.map (fun (start,offset) -> Interval (abs start,abs start+abs offset))

        static member StringWithoutNullChars =
            Default.String
            |> Gen.where (not << String.exists ((=) '\000'))
            |> Gen.map StringNoNulls

        static member NonEmptyString =
            Default.String
            |> Gen.where (fun s -> not (String.IsNullOrEmpty s) && not (String.exists ((=) '\000') s))
            |> Gen.map NonEmptyString

        static member Set() = 
            Default.FsList()
            |> Gen.map Set.ofList

        static member Map() = 
            Default.FsList()
            |> Gen.map Map.ofList

        static member NonEmptyArray() =
            Default.Array()
            |> Gen.where (fun a -> Array.length a > 0)
            |> Gen.map NonEmptyArray

        static member NonEmptySet() =
            Default.Set()
            |> Gen.where (not << Set.isEmpty) 
            |> Gen.map NonEmptySet

        /////Arrays whose length does not change when shrinking.
        //static member FixedLengthArray() =
        //    { new Arbitrary<'a[]>() with
        //        override __.Generator = generate
        //        override __.Shrinker a = a |> Seq.mapi (fun i x -> shrink x |> Seq.map (fun x' ->
        //                                                   let data' = Array.copy a
        //                                                   data'.[i] <- x'
        //                                                   data')
        //                                           ) |> Seq.concat
        //    }
        //    |> convert FixedLengthArray (fun (FixedLengthArray a) -> a)

        /// Generate a System.Collections.Generic.List of values.
        static member List() =
            Default.Array() 
            |> Gen.map Enumerable.ToList

        /// Generate a System.Collections.Generic.IList of values.
        static member IList() =
            Default.Array()
            |> Gen.map (fun x -> x :> _ IList)

        /// Generate a System.Collections.Generic.ICollection of values.
        static member ICollection() =
            Default.Array()
            |> Gen.map (fun x -> x :> _ ICollection)

        /// Generate a System.Collections.Generic.Dictionary of values.
        /// Shrinks by reducing the number of elements
        static member Dictionary() =
            //let genDictionary = 
                gen {
                    let! keys = Gen.arrayOf generate |> Gen.map (Seq.where (fun x -> not (obj.Equals(x, null))) >> Seq.distinct >> Seq.toList)
                    let! values = Gen.arrayOfLength keys.Length generate
                    return (Seq.zip keys values).ToDictionary(fst, snd)
                }
            //let shrinkDictionary (d: Dictionary<_,_>) = 
            //    let keys = Seq.toArray d.Keys
            //    seq {
            //        for c in keys.Length-2 .. -1 .. 0 ->
            //            let k = keys.[0..c]
            //            let values = Seq.truncate k.Length d.Values
            //            (Seq.zip k values).ToDictionary(fst, snd)
            //    }
            //fromGenShrink (genDictionary, shrinkDictionary)

        /// Generate a System.Collections.Generic.IDictionary of values.
        /// Shrinks by reducing the number of elements
        static member IDictionary() =
            Default.Dictionary()
            |> Gen.map (fun x -> x :> IDictionary<_,_>)

//        static member Culture() =
//#if NETSTANDARD1_6
//            let cultures = 
//                cultureNames |> Seq.choose (fun name -> try Some (CultureInfo name) with _ -> None)
//                      |> Seq.append [ CultureInfo.InvariantCulture; 
//                                      CultureInfo.CurrentCulture; 
//                                      CultureInfo.CurrentUICulture; 
//                                      CultureInfo.DefaultThreadCurrentCulture;
//                                      CultureInfo.DefaultThreadCurrentUICulture; ]
//            let genCulture = Gen.elements cultures
//#else
//            let genCulture = Gen.elements (CultureInfo.GetCultures (CultureTypes.NeutralCultures ||| CultureTypes.SpecificCultures))
//#endif
//            let shrinkCulture =
//                Seq.unfold <| fun c -> if c = null || c = CultureInfo.InvariantCulture || c.Parent = null
//                                            then None
//                                            else Some (c.Parent, c.Parent)
//            fromGenShrink (genCulture, shrinkCulture)

        static member Guid =
            (fun a b c d e f g h i j k -> Guid((a:int),b,c,d,e,f,g,h,i,j,k))
                <!> Default.Int32
                <*> Default.Int16 <*> Default.Int16
                <*> Default.Byte <*> Default.Byte <*> Default.Byte <*> Default.Byte 
                <*> Default.Byte <*> Default.Byte <*> Default.Byte <*> Default.Byte
            // TODO: turn off shrinking?

#if !NETSTANDARD1_6
        static member IPv4Address =
            //let generator =
                generate
                |> Gen.arrayOfLength 4
                |> Gen.map (IPAddress >> IPv4Address)
            //let shrinker (IPv4Address a) =
            //    a.GetAddressBytes()
            //    |> shrink
            //    |> Seq.filter (fun x -> Seq.length x = 4)
            //    |> Seq.map (IPAddress >> IPv4Address)
        
            //fromGenShrink (generator, shrinker)

        static member IPv6Address =
            //let generator =
                generate
                |> Gen.arrayOfLength 16
                |> Gen.map (IPAddress >> IPv6Address)
            //let shrinker (IPv6Address a) =
            //    a.GetAddressBytes()
            //    |> shrink
            //    |> Seq.filter (fun x -> Seq.length x = 16)
            //    |> Seq.map (IPAddress >> IPv6Address)
        
            //fromGenShrink (generator, shrinker)

        static member IPAddress =
            //let generator = 
            gen {
                let! byteLength = Gen.elements [4; 16]
                let! bytes = generate |> Gen.arrayOfLength byteLength
                return IPAddress bytes }
            //let shrinker (a:IPAddress) =
            //    a.GetAddressBytes()
            //    |> shrink
            //    |> Seq.filter (fun x -> Seq.length x = 4 || Seq.length x = 16)
            //    |> Seq.map IPAddress
        
            //fromGenShrink (generator, shrinker)
#endif

        static member HostName =
            let isValidSubdomain (subDomain: string) = String.IsNullOrWhiteSpace subDomain |> not && subDomain.Length <= 63 && subDomain.StartsWith("-") |> not && subDomain.EndsWith("-") |> not
            let isValidHost (host: string) = String.IsNullOrWhiteSpace host |> not && host.Length <= 255 && host.StartsWith(".") |> not && host.Split('.') |> Array.forall isValidSubdomain
            let subdomain = 
                gen {
                    let subdomainCharacters = "abcdefghijklmnopqrstuvwxyz0123456789-".ToCharArray()
                    let! subdomainLength = Gen.choose (1, 63)
                    return! 
                        Gen.elements subdomainCharacters 
                        |> Gen.arrayOfLength subdomainLength 
                        |> Gen.map String
                        |> Gen.filter isValidSubdomain
                }

            //let host = 
            gen {
                let! tld = Gen.elements topLevelDomains
                let! numberOfSubdomains = Gen.frequency [(20, Gen.constant 0); (4, Gen.constant 1); (2, Gen.constant 2); (1, Gen.constant 3)]
                
                return! 
                    Gen.listOfLength numberOfSubdomains subdomain
                    |> Gen.map (fun x -> x @ [tld] |> String.concat ".")
                    |> Gen.filter isValidHost
                    |> Gen.map HostName
            }

            //let shrinkHost (HostName host) =
            //    let parts = host.Split '.'
            //    let topLevelDomain = parts.[parts.Length - 1]

            //    seq {
            //        if parts.Length > 1 then
            //            yield parts.[1 ..] |> String.concat "."
            //        if Seq.exists (fun tld -> tld = topLevelDomain) commonTopLevelDomains |> not then
            //            yield! commonTopLevelDomains
            //                    |> Seq.map (fun tld -> Array.append parts.[0 .. parts.Length - 2] [|tld|] |> String.concat ".") }
            //    |> Seq.map HostName

            //fromGenShrink (host, shrinkHost)

#if NETSTANDARD1_6
#else
        static member MailAddress() =
            let isValidUser (user: string) = 
                String.IsNullOrWhiteSpace user |> not &&
                not (user.StartsWith("\"")) && 
                not (user.EndsWith("\"")) && 
                not (user.StartsWith(".")) && 
                not (user.EndsWith(".")) && 
                not (user.Contains(".."))

            let split (str: string) = 
                if String.IsNullOrWhiteSpace str || str.Length <= 1 then 
                    Seq.empty
                else seq {
                     yield str.[0 .. str.Length / 2 - 1]
                     yield str.[str.Length / 2 ..]
                }

            let createMailAddress name user host =
                if String.IsNullOrWhiteSpace name then
                    MailAddress (sprintf "%s@%s" user host)
                else 
                    MailAddress (sprintf "%s <%s@%s>" name user host)      

            let name = 
                gen {
                    let! localLength = Gen.choose (1, 63)

                    return! Gen.elements "abcdefghijklmnopqrstuvwxyz0123456789!#$%&'*+-/=?^_`.{|}~,:;[] "
                            |> Gen.arrayOfLength (localLength - 2) 
                            |> Gen.map String
                }

            let host = 
                Default.HostName
                |> Gen.map (fun (HostName h) -> h)

            let user = 
                gen {
                    let userChars = "abcdefghijklmnopqrstuvwxyz0123456789!#$%&'*+-/=?^_`{|}~"
                    let! localLength = Gen.choose (1, 63)

                    return! Gen.elements userChars
                            |> Gen.arrayOfLength localLength
                            |> Gen.map String
                            |> Gen.filter isValidUser
                }
            
            //let generator = 
            gen {
                let! name = name
                let! user = user
                let! host = host
                return createMailAddress name user host
            }

            //let shrinkDisplayName (a:MailAddress) =
            //    if String.IsNullOrWhiteSpace a.DisplayName then
            //        Seq.empty
            //    else seq {
            //        yield! a.DisplayName |> split |> Seq.map (fun displayName -> createMailAddress displayName a.User a.Host)
            //        yield createMailAddress "" a.User a.Host
            //    }

            //let shrinkUser (a:MailAddress) = 
            //    a.User |> split |> Seq.map (fun user -> createMailAddress a.DisplayName user a.Host)

            //let shrinkHost (a:MailAddress) = 
            //    Default.HostName().Shrinker (HostName a.Host)
            //    |> Seq.map (fun (HostName h) -> createMailAddress a.DisplayName a.User h)
            
            //let shrinker (a:MailAddress) = 
            //    seq {
            //        yield! shrinkDisplayName a
            //        yield! shrinkUser a
            //        yield! shrinkHost a
            //    } |> Seq.distinct
        
            //fromGenShrink (generator, shrinker)
#endif

        ///Arbitray instance for BigInteger.
        static member BigInt =
            Default.Int64
            |> Gen.map bigint

        ///Overrides the shrinker of any type to be empty, i.e. not to shrink at all.
        static member DoNotShrink() =
            generate |> Gen.withShrinkStream (fun _ -> Shrink.empty) |> Gen.map DoNotShrink
            
        ///Try to derive an arbitrary instance for the given type reflectively. 
        ///Generates and shrinks values for record, union, tuple and enum types.
        ///Also generates (but doesn't shrink) values for basic classes 
        ///(i.e. either classes having a single constructor with immutable values  
        ///or DTO classes with a default constructor and public property setters).
        static member Derive() =
            //taking out this generator makes sure that the memoization table in reflectGenObj
            //is used properly.
            let generator = ReflectArbitrary.reflectGenObj getGenerator
            
            generator typeof<'a> |> Gen.map unbox<'a>
            //{ new Arbitrary<'a>() with
            //    override __.Generator = generator typeof<'a> |> Gen.map unbox<'a>
            //    override __.Shrinker a = ReflectArbitrary.reflectShrink getShrink a
            //}
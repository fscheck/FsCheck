namespace FsCheck.Internals

open FsCheck
open FsCheck.FSharp
open System.Net
open System
open System.Net.Mail
open System.Collections.Generic
open System.Linq
open System.Globalization


/// A collection of default Arbitrary instances for some types.
[<Sealed;AbstractClass>]
type internal Default private() =

    // Design note:
    // using static member val Foo = .. with get is auto-property syntax,
    // which means that each of these Arbitrary instances should only be created once,
    // and at initialization of this type.
    // This is a bit wonky with type inference though.
    // 1. Often a return type annotation is needed if the property is used elsewhere in the
    //    class, even though the type is inferred correctly.
    // 2. F# is not happy if you try to refer to a generic method
    //    in the initializer, e.g. Default.Array(Default.Char) then raises several
    //    "code not as generic as indicated by type annotations" on Default.Array, and that
    //     method has then been constrained to work for chars only. 
    //    So for the moment, generic methods are on Arb module too to use the from there.
    //    This may be a limitation of static initializers in .NET?

    /// Generates (), of the unit type.
    static member val Unit =
            Gen.constant ()
            |> Arb.fromGen
        with get

    /// Generates bool values.
    static member val Bool :Arbitrary<bool> = 
            Gen.elements [false; true]
            |> Arb.fromGen
        with get

    /// Generates byte values that are uniformly distributed in the whole range of byte values.
    static member val Byte :Arbitrary<byte> = 
            (Gen.choose (int Byte.MinValue, int Byte.MaxValue) |> Gen.map byte,
             Shrink.unsignedNumber)
             |> Arb.fromGenShrink
        with get

    /// Generates sbyte values that are uniformly distributed in the whole range of sbyte values.
    static member val SByte =
            (Gen.choose (int SByte.MinValue, int SByte.MaxValue) |> Gen.map sbyte,
             Shrink.signedNumber)
            |> Arb.fromGenShrink
        with get

    /// Generates int16 values that are between -size and size.
    static member val Int16 =
            (Gen.sized( fun s -> Gen.choose(-s, s) |> Gen.map int16),
             Shrink.signedNumber)
            |> Arb.fromGenShrink
        with get

    /// Generate int16 values that are uniformly distributed in the whole range of int16 values.
    static member val DoNotSizeInt16 :Arbitrary<DoNotSize<int16>> = 
            (Gen.choose(int Int16.MinValue, int Int16.MaxValue) |> Gen.map int16,
             Shrink.signedNumber)
            |> Arb.fromGenShrink
            |> Arb.convert DoNotSize DoNotSize.Unwrap 
        with get

    /// Generates uint16 values that are between 0 and size.
    static member val UInt16 =
            (Gen.sized( fun s -> Gen.choose(0, s) |> Gen.map uint16),
             Shrink.unsignedNumber)
            |> Arb.fromGenShrink
        with get

    /// Generate arbitrary uint16 that is uniformly distributed in the whole range of uint16 values.
    static member val DoNotSizeUInt16 =
            (Gen.choose(int UInt16.MinValue, int UInt16.MaxValue) |> Gen.map uint16, 
             Shrink.unsignedNumber)
            |> Arb.fromGenShrink
            |> Arb.convert DoNotSize DoNotSize.Unwrap 
        with get
         
    /// Generates int32 values that are between -size and size.
    static member val Int32 =
            (Gen.sized (fun n -> Gen.choose (-n,n)),
             Shrink.signedNumber)
            |> Arb.fromGenShrink
        with get

    ///Generate arbitrary int32 that is unrestricted by size.
    static member val DoNotSizeInt32 =
            (Gen.choose(Int32.MinValue, Int32.MaxValue),
             Shrink.signedNumber)
            |> Arb.fromGenShrink
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get

    ///Generates uint32 values that are between 0 and size.
    static member val UInt32 =
            (Gen.sized( fun s -> Gen.choose(0, s) |> Gen.map uint32),
             Shrink.unsignedNumber)
            |> Arb.fromGenShrink
        with get

    ///Generate arbitrary uint32 that is unrestricted by size.
    static member val DoNotSizeUInt32 =
            (Gen.choose64(int64 UInt32.MinValue, int64 UInt32.MaxValue) |> Gen.map uint32,
             Shrink.unsignedNumber)
            |> Arb.fromGenShrink
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get

    ///Generates int64 values that are between -size and size.
    ///Note that since the size is an int32, this does not actually cover the full
    ///range of int64. See DoNotSize<int64> instead.
    static member val Int64 =
            (Gen.sized( fun s -> Gen.choose64(int64 -s, int64 s)),
             Shrink.signedNumber)
            |> Arb.fromGenShrink
        with get

    ///Generate arbitrary int64 that is unrestricted by size.
    static member val DoNotSizeInt64 : Arbitrary<DoNotSize<int64>> =
            (Gen.choose64(Int64.MinValue, Int64.MaxValue),
             Shrink.signedNumber)
            |> Arb.fromGenShrink
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get
     
    ///Generates uint64 values that are between 0 and size.
    static member val UInt64 =
            (Gen.sized( fun s -> Gen.choose(0, s) |> Gen.map uint64),
             Shrink.unsignedNumber)
            |> Arb.fromGenShrink
        with get
     
    ///Generates uint64 values that are unrestricted by size.
    static member val DoNotSizeUInt64 =
            (Default.DoNotSizeInt64.Generator |> Gen.map (fun (DoNotSize i64) -> Operators.uint64 i64),
             Shrink.unsignedNumber)
            |> Arb.fromGenShrink
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get

    ///Generates float values that are between -size and size (without NaN, Infinity, Epsilon, MinValue, MaxValue)
    ///Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val NormalFloat :Arbitrary<NormalFloat> =
            let generator = Gen.sized (fun size ->
                Gen.map2
                    (fun f isNegative -> 
                        let value = f * float size
                        if isNegative then -value else value)
                    Gen.double
                    Default.Bool.Generator
                )
            let shrinker fl =
                let (|<|) x y = abs x < abs y
                seq { 
                        if fl <> 0.0 then yield 0.0
                        if fl < 0.0 then yield -fl
                        let truncated = truncate fl
                        if truncated |<| fl then yield truncated }
                |> Seq.distinct
            (generator, shrinker)
            |> Arb.fromGenShrink
            |> Arb.convert NormalFloat float
        with get

    /// Generates float values that are between -size and size.
    /// One of NaN, NegativeInfinity, PositiveInfinity, MaxValue, MinValue or Epsilon is generated on average 1 time for every 8 values.
    /// Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val Float = 
            let generator =
                Gen.frequency [(7, Default.NormalFloat.Generator |> Gen.map (fun (NormalFloat f) -> f))
                               (1, Gen.elements [ Double.NaN; Double.NegativeInfinity; Double.PositiveInfinity
                                                  Double.MaxValue; Double.MinValue; Double.Epsilon])]
            let shrinker fl =
                if Double.IsInfinity fl || Double.IsNaN fl then Seq.singleton 0.0
                else Default.NormalFloat.Shrinker (NormalFloat fl) |> Seq.map (fun (NormalFloat f) -> f)
            Arb.fromGenShrink(generator, shrinker)
        with get

    /// Generates float values that are between -size and size.
    /// One of NaN, NegativeInfinity, PositiveInfinity, MaxValue, MinValue or Epsilon is generated on average 1 time for every 8 values.
    /// Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val Float32 = 
            let generator =
                Gen.frequency [(7, Default.NormalFloat.Generator |> Gen.map (fun (NormalFloat f) -> float32 f))
                               (1, Gen.elements [ Single.NaN; Single.NegativeInfinity; Single.PositiveInfinity
                                                  Single.MaxValue; Single.MinValue; Single.Epsilon])]
            let shrinker fl =
                if Single.IsInfinity fl || Single.IsNaN fl then seq {yield 0.0f}
                else Default.NormalFloat.Shrinker (NormalFloat <| float fl) |> Seq.map (fun (NormalFloat f) -> float32 f)
            Arb.fromGenShrink (generator,shrinker)
        with get

    /// Generates decimal values that are between -size and size.
    // /Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val Decimal :Arbitrary<decimal> =
            /// Generates uniformly distributed Decimal values in range [0; 1).
            let decimalGen =
                let tenPow9 = 1_000_000_000
                Gen.choose(0, tenPow9 - 1)
                |> Gen.map Decimal
                |> Gen.map (fun d -> d / (Decimal tenPow9)) 
            let generator = Gen.sized (fun size ->
                decimalGen
                |> Gen.map (fun d -> d * Decimal(size)))
            let shrinker d =
                let (|<|) x y = abs x < abs y
                seq {
                    if d <> 0m then yield 0m
                    if d < 0m then yield -d
                    let truncated = truncate d
                    if truncated |<| d then yield truncated
                }
            (generator, shrinker)
            |> Arb.fromGenShrink
        with get

    ///Generates decimal values that are unrestricted by size.
    ///Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val DoNotSizeDecimal =
            let int32Gen = Default.DoNotSizeInt32 |> Arb.toGen |> Gen.map (fun (DoNotSize i) -> i)
            let generator = 
                Gen.map5 
                    (fun lo mid hi isNegative scale -> Decimal(lo, mid, hi, isNegative, scale))
                    int32Gen
                    int32Gen
                    int32Gen
                    Default.Bool.Generator
                    (Gen.choose(0, 28) |> Gen.map Operators.byte)
            (generator, Default.Decimal.Shrinker)
            |> Arb.fromGenShrink
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get

    ///Generates complex values of form {float + i*float}. 
    ///Shrinks by removing the imaginary part and shrinking both parts.
    static member val Complex =
            let arbFloat = Default.Float
            let arbFloat2 = Arb.zip (arbFloat, arbFloat)
            let generator = arbFloat2.Generator
                            |> Gen.map Numerics.Complex
            let shrinker (c : Numerics.Complex) =
                match (c.Real, c.Imaginary) with
                | (r, 0.0) -> 
                    arbFloat.Shrinker r
                    |> Seq.map (fun r -> Numerics.Complex(r, 0.0))
                | (r, i) ->
                    let realOnly = seq { yield Numerics.Complex(r, 0.0)}
                    let shrunk =
                        arbFloat2.Shrinker (r, i)
                        |> Seq.filter (fun (sr, si) -> not (si = 0.0 && sr.Equals r)) //We use Equals to properly compare NaNs
                        |> Seq.map Numerics.Complex
                    Seq.append realOnly shrunk
            Arb.fromGenShrink (generator, shrinker)
        with get

    /// Generates characters that are between ASCII codes Char.MinValue and 127.
    static member val Char =
            let generator = Gen.choose (int Char.MinValue, 127) |> Gen.map char
            let shrinker c = seq { for c' in ['a';'b';'c'] do if c' < c || not (Char.IsLower c) then yield c' }
            (generator, shrinker)
            |> Arb.fromGenShrink
        with get

    ///Generates strings, which are lists of characters or null (1/10 of the time).
    static member val String :Arbitrary<string> =
            let arrayOfChars = Arb.array Default.Char
            let generator = 
                arrayOfChars.Generator 
                |> Gen.map String
            let shrinker (s:string) = 
                    match s with
                    | null -> Seq.empty<string>
                    | _ -> s.ToCharArray() |> arrayOfChars.Shrinker |> Seq.map String
            Arb.fromGenShrink (generator, shrinker)
        with get

    ///Generates a rank 1 arrays. 
    ///The length of the generated array is between 0 and the test size + 1. 
    ///The sum of the sizes of the elements is equal to the size of the generated array.
    static member Array(elements: Arbitrary<'T>) =
        elements
        |> Arb.array

        
    ///Generates option values that are 'None' 1/8 of the time.
    static member Option(value: Arbitrary<'T>) = 
        value
        |> Arb.option

    ///Generates underlying values that are not null.
    static member NonNull(value: Arbitrary<'T>) =
        let inline notNull x = not (isNull x)
        value
        |> Arb.filter notNull
        |> Arb.convert NonNull (fun (NonNull x) -> x)


    ///Generates nullable values that are null 1/8 of the time.
    static member Nullable(value: Arbitrary<'T>) = 
        value
        |> Arb.nullable

    ///Generates F# (immutable) lists. 
    ///The length of the generated list is between 0 and the test size + 1. 
    ///The sum of the sizes of the elements is equal to the size of the generated list.
    static member FsList(elements: Arbitrary<'T>) = 
        elements
        |> Arb.list

    ///Generates objects which are a boxed char, string or boolean value.
    static member val Object =
            let (arbChar, arbString, arbBool) = Default.Char, Default.String, Default.Bool
            let generator = 
                Gen.oneof [ Gen.map box <| arbChar.Generator
                            Gen.map box <| arbString.Generator
                            Gen.map box <| arbBool.Generator ]
            let shrinker (o:obj) =
                if isNull o then
                    Seq.empty
                else
                    seq {
                        match o with
                        | :? char as c -> yield box true; yield box false; yield! arbChar.Shrinker c |> Seq.map box
                        | :? string as s -> yield box true; yield box false; yield! arbString.Shrinker s |> Seq.map box
                        | :? bool -> yield! Seq.empty
                        | _ -> failwithf "Unknown type in shrink of obj type - object is %A" o
                    }
            Arb.fromGenShrink (generator, shrinker)
        with get



    ///Generate a rank 2, zero based array. 
    ///The product of the width and the height is between 0 and the test size.
    // {NOT YET} The sum of the sizes of the elements is equal to the size of the generated array.
    static member Array2D(elements: Arbitrary<'T>) = 
        let shrinkArray2D (arr:_[,]) =
            let removeRow r (arr:_[,]) =
                Array2D.init (Array2D.length1 arr-1) (Array2D.length2 arr) (fun i j -> if i < r then arr.[i,j] else arr.[i+1,j])
            let removeCol c (arr:_[,]) =
                Array2D.init (Array2D.length1 arr) (Array2D.length2 arr-1) (fun i j -> if j < c then arr.[i,j] else arr.[i,j+1])
            seq { for r in 1..Array2D.length1 arr do yield removeRow r arr
                  for c in 1..Array2D.length2 arr do yield removeCol c arr
                  for i in 0..Array2D.length1 arr-1 do //hopefully the matrix is shrunk considerably before we get here...
                    for j in 0..Array2D.length2 arr-1 do
                        for elem in elements.Shrinker arr.[i,j] do
                            let shrunk = Array2D.copy arr
                            shrunk.[i,j] <- elem
                            yield shrunk
                }       
        Arb.fromGenShrink (elements.Generator |> Gen.array2DOf, shrinkArray2D)
           

    // NOTE: Function types are a bit special in that they need to have explicit type parameters.
    // Otherwise the discovery of the default arbitrary map will not work correctly, as it assumes
    // that if looking for a Arbitrary<FSharpFunc<int,string>> it can instantiate the generic method with
    // int and string in that order. Due to the "target" parameter however, the order of the generic parameters
    // is reversed if not given explicitly, so you end with a function from target to domain.

    /// Generates F# function values. Function values can be generated for types 'T->'U where the target domain 'U has an Arbitrary instance.
    /// There is no shrinking for function values.
    [<CompiledName("FSharpFunc")>]
    static member FSharpFunc<'T,'U when 'T:equality>(target: Arbitrary<'U>) :Arbitrary<'T->'U> = 
        target.Generator
        |> Gen.pureFunction
        |> Arb.fromGen

    ///Generates F# function values that generate an instance of the function result type about half the time. The other 
    ///times it generate one of a list of common .NET exceptions, including Exception, ArgumentException, ArithmeticException,
    ///IOException, NotImplementedException, OUtOfMemoryException and others.
    [<CompiledName("ThrowingFSharpFunc")>]
    static member ThrowingFSharpFunc<'T,'U when 'T:equality>(target: Arbitrary<'U>) :Arbitrary<ThrowingFunction<'T,'U>> = 
        let exns: exn array = [|Exception()

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

                                NotFiniteNumberException()
                                StackOverflowException()

                                IO.DirectoryNotFoundException()
                                IO.FileLoadException()
                                KeyNotFoundException()
                                IO.PathTooLongException()
                            |]
        target |> Arb.throwingFunction exns |> Arb.convert ThrowingFunction (fun tf -> tf.Get)

    ///Generates Function values that can be printed and shrunk. Function values can be generated for types 'T->'U
    ///where 'b has an Arbitrary instance.
    static member Function<'T,'U when 'T:equality and 'T:comparison>(target: Arbitrary<'U>) : Arbitrary<Function<'T,'U>> =
        let generator =
            target.Generator
            |> Gen.pureFunction 
            |> Gen.map Function<_,_>.From
        let shrinker (f:Function<_, _>) = 
            let update x' y' f x = if x = x' then y' else f x
            seq { for (x,y) in f.Table do 
                    for y' in target.Shrinker y do 
                        yield Function<_,_>.From (update x y' f.Value) }
        Arb.fromGenShrink(generator, shrinker)

    ///Generates Func'1 values.
    static member SystemFunc(target: Arbitrary<'U>) =
        Default.FSharpFunc(target)
        |> Arb.convert (fun f -> Func<_>(f)) (fun f -> f.Invoke)

    ///Generates Func'2 values.
    static member SystemFunc1(target: Arbitrary<'U>) =
        target
        |> Default.FSharpFunc
        |> Arb.convert (fun f -> Func<_,_>(f)) (fun f -> f.Invoke)

    ///Generates Func'3 values.
    static member SystemFunc2(target: Arbitrary<'U>) =
        target
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun f -> Func<_,_,_>(f)) (fun f a b -> f.Invoke(a,b))

    ///Generates Func'4 values.
    static member SystemFunc3(target: Arbitrary<'U>) =
        target
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun f -> Func<_,_,_,_>(f)) (fun f a b c -> f.Invoke(a,b,c))

    ///Generates Action'0 values.
    static member SystemAction() =
        Default.Unit
        |> Default.FSharpFunc
        |> Arb.convert (fun f -> Action(f)) (fun f -> f.Invoke)

    ///Generates Action'1 values.
    static member SystemAction1() =
        Default.Unit
        |> Default.FSharpFunc
        |> Arb.convert (fun f -> Action<_>(f)) (fun f -> f.Invoke)

    ///Generates Action'2 values.
    static member SystemAction2() =
        Default.Unit
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun f -> Action<_,_>(f)) (fun f a b -> f.Invoke(a,b))

    ///Generates Action'3 values.
    static member SystemAction3() =
        Default.Unit
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun f -> Action<_,_,_>(f)) (fun f a b c -> f.Invoke(a,b,c))

    ///Generates DateTime values that are between 1900 and 2100. 
    ///A DateTime is shrunk by removing its Kind, millisecond, second, minute and hour components.
    static member val DateTime :Arbitrary<DateTime> =
            let genDate = 
                gen {   let! y = Gen.choose(1900,2100)
                        and! m = Gen.choose(1, 12)
                        let! d = Gen.choose(1, DateTime.DaysInMonth(y, m))
                        and! h = Gen.choose(0,23)
                        and! min = Gen.choose(0,59)
                        and! sec = Gen.choose(0,59)
                        and! ms = Gen.choose(0,999)
                        and! kind = Gen.elements [DateTimeKind.Unspecified; DateTimeKind.Utc; DateTimeKind.Local]
                        return DateTime(y, m, d, h, min, sec, ms, kind) }
            Arb.fromGenShrink (genDate, Shrink.date)
        with get

    ///Generates DateTime values that are unrestricted by size.
    ///A DateTime is shrunk by removing its Kind, millisecond, second, minute and hour components.
    static member val DoNotSizeDateTime =
            let genDate = gen { let! (DoNotSize ticks) = Default.DoNotSizeInt64.Generator
                                and! kind = Gen.elements [DateTimeKind.Unspecified; DateTimeKind.Utc; DateTimeKind.Local]
                                return DateTime(abs ticks, kind) }
            Arb.fromGenShrink (genDate, Shrink.date)
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get

    ///Generates TimeSpan values that are unrestricted by size. 
    ///A TimeSpan is shrunk by removing days, hours, minutes, second and milliseconds.
    static member val TimeSpan :Arbitrary<TimeSpan> =
            let genTimeSpan = Default.DoNotSizeInt64.Generator |> Gen.map (fun (DoNotSize i) -> TimeSpan i)
            let shrink (t: TimeSpan) = 
                if t.Days <> 0 then
                    seq { yield TimeSpan(0, t.Hours, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Hours <> 0 then
                    seq { yield TimeSpan(0, 0, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Minutes <> 0 then
                    seq { yield TimeSpan(0, 0, 0, t.Seconds, t.Milliseconds) }
                elif t.Seconds <> 0 then
                    seq { yield TimeSpan(0, 0, 0, 0, t.Milliseconds) }
                elif t.Milliseconds <> 0 then
                    seq { yield TimeSpan.Zero }
                else
                    Seq.empty
            Arb.fromGenShrink (genTimeSpan, shrink)
        with get

    ///Generates DateTimeOffset values that are between 1900 and 2100. 
    /// A DateTimeOffset is shrunk first by shrinking its offset, then by removing its second, minute and hour components.
    static member val DateTimeOffset =
            let genTimeZone = gen {
                let! hours = Gen.choose(-12, 14)
                let! minutes =
                    if hours = -12 || hours = 14 then
                        Gen.constant 0
                    else 
                        Gen.choose(0, 59)
                return TimeSpan(hours, minutes, 0) }
            let arbTimeSpan = Default.TimeSpan
            let shrinkTimeZone (t: TimeSpan) =
                arbTimeSpan.Shrinker t |> Seq.where (fun z -> z.Hours > 0 || z.Minutes > 0)
            let genDateTime = Default.DateTime.Generator
            let genDate = gen { 
                let! t = genDateTime
                and! tz = genTimeZone
                return DateTimeOffset(DateTime.SpecifyKind(t, DateTimeKind.Unspecified), tz) }
            let shrinkDateTimeOffset (d: DateTimeOffset) =
                seq {
                    for ts in shrinkTimeZone d.Offset ->
                        DateTimeOffset(d.DateTime, ts)
                    if d.Offset <> TimeSpan.Zero then
                        yield DateTimeOffset(d.DateTime, TimeSpan.Zero)
                    for dt in Default.DateTime.Shrinker d.DateTime ->
                        DateTimeOffset(dt, TimeSpan.Zero) }
            Arb.fromGenShrink (genDate, shrinkDateTimeOffset)
        with get

    static member KeyValuePair(key: Arbitrary<'K>, value: Arbitrary<'V>) =
        let genKeyValuePair =
            Gen.map2 (fun k v -> KeyValuePair(k, v)) key.Generator value.Generator
        let shrinkKeyValuePair (kvp:KeyValuePair<_,_>) = 
            seq { for key in key.Shrinker kvp.Key do
                    for value in value.Shrinker kvp.Value do
                        yield new KeyValuePair<_,_>(key, value) }
        Arb.fromGenShrink(genKeyValuePair,shrinkKeyValuePair)

    static member val NegativeInt =
            Default.Int32
            |> Arb.mapFilter (fun x -> -abs x) (fun x -> x < 0) // fsharplint:disable-line CanBeReplacedWithComposition
            |> Arb.convert NegativeInt int
        with get

    static member val NonNegativeInt =
            Default.Int32
            |> Arb.mapFilter abs (fun i -> i >= 0)
            |> Arb.convert NonNegativeInt int
        with get

    static member val PositiveInt =
            Default.Int32
            |> Arb.mapFilter abs (fun i -> i > 0)
            |> Arb.convert PositiveInt int
        with get

    static member val NonZeroInt =
            Default.Int32
            |> Arb.filter ((<>) 0)
            |> Arb.convert NonZeroInt int
        with get

    static member val IntWithMinMax =
            let arbInt32 = Default.Int32
            let generator = Gen.frequency [(1,  Gen.elements [Int32.MaxValue; Int32.MinValue])
                                           (10, arbInt32.Generator) ] 
                            |> Gen.map IntWithMinMax
            let shrinker (IntWithMinMax i) = arbInt32.Shrinker i |> Seq.map IntWithMinMax
            Arb.fromGenShrink(generator, shrinker)
        with get

    ///Generates intervals between two non-negative integers.
    static member val Interval =
            let arbInt32 = Default.Int32
            let arbTuple = Arb.zip(arbInt32, arbInt32)
            let generator = 
                arbInt32.Generator
                |> Gen.two
                |> Gen.map (fun (start, offset) -> Interval (abs start, abs start + abs offset))
            let shrinker (i : Interval) =
                (i.Left, i.Right - i.Left)
                |> arbTuple.Shrinker
                |> Seq.map (fun (start, offset) -> Interval(start, start + offset))
            Arb.fromGenShrink(generator, shrinker)
        with get

    static member val  StringNoNulls =
            Default.String
            |> Arb.filter (not << String.exists ((=) '\000'))
            |> Arb.convert StringNoNulls string
        with get

    static member val NonEmptyString =
            Default.String
            |> Arb.filter (fun s -> not (String.IsNullOrEmpty s) && not (String.exists ((=) '\000') s))
            |> Arb.convert NonEmptyString string
        with get

    static member val NonWhiteSpaceString =
            Default.String
            |> Arb.filter (fun s -> not (String.IsNullOrWhiteSpace s) && not (String.exists ((=) '\000') s))
            |> Arb.convert NonWhiteSpaceString string
        with get

    static member val XmlEncodedString =
            Default.String
            |> Arb.mapFilter 
                System.Net.WebUtility.HtmlEncode
                (String.forall Xml.XmlConvert.IsXmlChar)
            |> Arb.convert XmlEncodedString string
        with get

    static member Set(elements: Arbitrary<'T>) = 
        Default.FsList(elements)
        |> Arb.convert Set.ofList Set.toList

    static member Map(keys: Arbitrary<'K>, values: Arbitrary<'V>) = 
        Default.FsList(Arb.zip(keys, values))
        |> Arb.convert Map.ofList Map.toList

    static member NonEmptyArray(elements: Arbitrary<'T>) =
        Default.Array(elements)
        |> Arb.filter (fun a -> Array.length a > 0)
        |> Arb.convert NonEmptyArray (fun (NonEmptyArray s) -> s)

    static member NonEmptySet(elements: Arbitrary<'T>) =
        Default.Set(elements)
        |> Arb.filter (not << Set.isEmpty) 
        |> Arb.convert NonEmptySet (fun (NonEmptySet s) -> s)

    ///Arrays whose length does not change when shrinking.
    static member FixedLengthArray(elements: Arbitrary<'T>) =
        let arbArray = Default.Array(elements)
        let generator = arbArray.Generator
        Arb.fromGenShrink (generator, Shrink.arrayElements elements.Shrinker)
        |> Arb.convert FixedLengthArray (fun (FixedLengthArray a) -> a)

    /// Generates System.Collections.Generic.List instances.
    static member List(elements: Arbitrary<'T>) =
        Default.Array(elements)
        |> Arb.convert Enumerable.ToList Seq.toArray

    /// Generates System.Collections.Generic.IList instances.
    static member IList(elements: Arbitrary<'T>) =
        Default.List(elements)
        |> Arb.convert (fun x -> x :> _ IList) (fun x -> x :?> _ List)

    /// Generates System.Collections.Generic.ICollection instances.
    static member ICollection(elements: Arbitrary<'T>) =
        Default.List(elements)
        |> Arb.convert (fun x -> x :> _ ICollection) (fun x -> x :?> _ List)

    /// Generates System.Collections.Generic.Dictionary instances.
    static member Dictionary(keys: Arbitrary<'K>, values: Arbitrary<'V>) =
        let genDictionary = 
            gen {
                let! keys = keys.Generator |> Gen.arrayOf |> Gen.map (Seq.where (fun x -> not (obj.Equals(x, null))) >> Seq.distinct >> Seq.toArray)
                let! values = values.Generator |> Gen.arrayOfLength keys.Length
                return (Seq.zip keys values).ToDictionary(fst, snd)
            }
        let shrinkDictionary (d: Dictionary<_,_>) = 
            let keys = Seq.toArray d.Keys
            seq {
                for c in keys.Length-2 .. -1 .. 0 ->
                    let k = keys.[0..c]
                    let values = Seq.truncate k.Length d.Values
                    (Seq.zip k values).ToDictionary(fst, snd)
            }
        Arb.fromGenShrink (genDictionary, shrinkDictionary)

    /// Generates System.Collections.Generic.IDictionary instances.
    static member IDictionary(keys: Arbitrary<'K>, values: Arbitrary<'V>) =
        Default.Dictionary(keys, values)
        |> Arb.convert (fun x -> x :> IDictionary<_,_>) (fun x -> x :?> Dictionary<_,_>)

    static member val Culture =
            let genCulture = Gen.elements (CultureInfo.GetCultures (CultureTypes.NeutralCultures ||| CultureTypes.SpecificCultures))
            let shrinkCulture =
                Seq.unfold <| fun c -> if (isNull c) || c = CultureInfo.InvariantCulture || (isNull c.Parent)
                                            then None
                                            else Some (c.Parent, c.Parent)
            Arb.fromGenShrink (genCulture, shrinkCulture)
        with get

    static member val Guid =
            let int32Gen = Default.DoNotSizeInt32.Generator |> Gen.map (fun (DoNotSize i) -> i)
            let int16Gen = Default.DoNotSizeInt16.Generator |> Gen.map (fun (DoNotSize i) -> i)
            let byteGen = Default.Byte.Generator
            gen {
                let! a = int32Gen
                and! b = int16Gen
                and! c = int16Gen
                and! d = byteGen
                and! e = byteGen
                and! f = byteGen
                and! g = byteGen
                and! h = byteGen
                and! i = byteGen
                and! j = byteGen
                and! k = byteGen
                return Guid((a: int),b,c,d,e,f,g,h,i,j,k) 
            } 
            |> Arb.fromGen
        with get

    ///Generates System.ConsoleKeyInfo values.
    ///Shrinks by reducing number of special key modifiers
    static member val ConsoleKeyInfo =
            let genBool, genChar = Default.Bool.Generator, Default.Char.Generator
            let generator = 
                gen {
                    let! char = genChar
                    and! key = ReflectiveGenerator.enumOfType (typeof<ConsoleKey>)
                    and! shift = genBool
                    and! alt = genBool
                    and! ctrl = genBool
                    return ConsoleKeyInfo(char, key :?> ConsoleKey, shift, alt, ctrl)
                }
            let shrinker (cki : ConsoleKeyInfo) =
                let toBools (m : ConsoleModifiers) = (
                    (m &&& ConsoleModifiers.Shift) <> enum<ConsoleModifiers>(0),
                    (m &&& ConsoleModifiers.Alt) <> enum<ConsoleModifiers>(0),
                    (m &&& ConsoleModifiers.Control) <> enum<ConsoleModifiers>(0))
                let (shift, alt, ctrl) = toBools cki.Modifiers
                seq {
                    if shift then yield (false, alt, ctrl)
                    if alt then yield (shift, false, ctrl)
                    if ctrl then yield (shift, alt, false)
                }
                |> Seq.map (fun (shift, alt, ctrl) -> 
                    ConsoleKeyInfo(cki.KeyChar, cki.Key, shift, alt, ctrl))
            Arb.fromGenShrink(generator, shrinker)
        with get

    static member val IPv4Address =
            let arbByte = Default.Byte
            let generator =
                arbByte.Generator
                |> Gen.arrayOfLength 4
                |> Gen.map (IPAddress >> IPv4Address)
            let shrinker (IPv4Address a) =
                a.GetAddressBytes()
                |> Shrink.arrayElements arbByte.Shrinker
                |> Seq.map (IPAddress >> IPv4Address)
     
            Arb.fromGenShrink (generator, shrinker)
        with get

    static member val IPv6Address =
            let arbByte = Default.Byte
            let generator =
                arbByte.Generator
                |> Gen.arrayOfLength 16
                |> Gen.map (IPAddress >> IPv6Address)
            let shrinker (IPv6Address a) =
                a.GetAddressBytes()
                |> Shrink.arrayElements arbByte.Shrinker
                |> Seq.map (IPAddress >> IPv6Address)
     
            Arb.fromGenShrink (generator, shrinker)
        with get

    static member val IPAddress =
            let arbByte = Default.Byte
            let generator = gen {
                let! byteLength = Gen.elements [4; 16]
                let! bytes = arbByte.Generator |> Gen.arrayOfLength byteLength
                return IPAddress bytes }
            let shrinker (a:IPAddress) =
                a.GetAddressBytes()
                |> Shrink.arrayElements arbByte.Shrinker
                |> Seq.map IPAddress
     
            Arb.fromGenShrink (generator, shrinker)
        with get

    static member val HostName :Arbitrary<HostName> =
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

            let host = 
                gen {
                    let! tld = Gen.elements Data.topLevelDomains
                    let! numberOfSubdomains = Gen.frequency [(20, Gen.constant 0); (4, Gen.constant 1); (2, Gen.constant 2); (1, Gen.constant 3)]
             
                    return! 
                        Gen.listOfLength numberOfSubdomains subdomain
                        |> Gen.map (fun x -> x @ [tld] |> String.concat ".")
                        |> Gen.filter isValidHost
                        |> Gen.map HostName
                }

            let shrinkHost (HostName host) =
                let parts = host.Split '.'
                let topLevelDomain = parts.[parts.Length - 1]

                seq {
                    if parts.Length > 1 then
                        yield parts.[1 ..] |> String.concat "."
                    if Seq.exists (fun tld -> tld = topLevelDomain) Data.commonTopLevelDomains |> not then
                        yield! Data.commonTopLevelDomains
                                |> Seq.map (fun tld -> Array.append parts.[0 .. parts.Length - 2] [|tld|] |> String.concat ".") }
                |> Seq.map HostName

            Arb.fromGenShrink (host, shrinkHost)
        with get

    static member val MailAddress =
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
                Default.HostName.Generator
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
         
            let generator = 
                gen {
                    let! name = name
                    let! user = user
                    let! host = host
                    return createMailAddress name user host
                }

            let shrinkDisplayName (a:MailAddress) =
                if String.IsNullOrWhiteSpace a.DisplayName then
                    Seq.empty
                else seq {
                    yield! a.DisplayName |> split |> Seq.map (fun displayName -> createMailAddress displayName a.User a.Host)
                    yield createMailAddress "" a.User a.Host
                }

            let shrinkUser (a:MailAddress) = 
                a.User |> split |> Seq.map (fun user -> createMailAddress a.DisplayName user a.Host)

            let shrinkHost (a:MailAddress) = 
                Default.HostName.Shrinker (HostName a.Host)
                |> Seq.map (fun (HostName h) -> createMailAddress a.DisplayName a.User h)
         
            let shrinker (a:MailAddress) = 
                seq {
                    yield! shrinkDisplayName a
                    yield! shrinkUser a
                    yield! shrinkHost a
                } |> Seq.distinct
     
            Arb.fromGenShrink (generator, shrinker)
        with get

    ///Generates BigInteger values that are between -size and size.
    static member val BigInt = Default.Int32 |> Arb.convert bigint int with get

    ///Overrides the shrinker of any type to be empty, i.e. not to shrink at all.
    static member DoNotShrink(any: Arbitrary<'T>) =
        any.Generator
        |> Gen.map DoNotShrink
        |> Arb.fromGen

    ///Try to derive an arbitrary instance for the given type reflectively. 
    ///Generates and shrinks values for record, union, tuple and enum types.
    ///Also generates (but doesn't shrink) values for basic classes 
    ///(i.e. either classes having a single constructor with immutable values  
    ///or DTO classes with a default constructor and public property setters).
    static member Derive<'T>(arbMap: IArbMap) =
        let getGenerator t =
            let arb = arbMap.ArbFor t
            arb.Generator
        let getShrink t =
            let arb = arbMap.ArbFor t
            arb.Shrinker
                
        let generator = 
            typeof<'T>
            |> ReflectiveGenerator.reflectGenObj getGenerator
            |> Gen.map unbox<'T>
        let shrinker a =
            ReflectiveShrinker.reflectShrink getShrink a
        Arb.fromGenShrink(generator, shrinker)



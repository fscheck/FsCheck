
///Generates Function values that can be printed and shrunk. Function values can be generated for types 'T->'U
///where 'b has an Arbitrary instance.///Generates Function values that can be printed and shrunk. Function values can be generated for types 'T->'U
///where 'b has an Arbitrary instance.///Generates Function values that can be printed and shrunk. Function values can be generated for types 'T->'U
///where 'b has an Arbitrary instance.
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
            Arb.constant ()
        with get

    /// Generates bool values.
    static member val Bool :Arbitrary<bool> = 
            Arb.bool
        with get

    /// Generates byte values that are uniformly distributed in the whole range of byte values.
    static member val Byte :Arbitrary<byte> = 
            Gen.choose (int Byte.MinValue, int Byte.MaxValue)
            |> Gen.map (byte >> Shrink.ofShrinker Shrink.unsignedNumber id)
            |> Arbitrary
        with get

    /// Generates sbyte values that are uniformly distributed in the whole range of sbyte values.
    static member val SByte :Arbitrary<sbyte> =
            Gen.choose (int Byte.MinValue, int Byte.MaxValue)
            |> Gen.map (sbyte >> Shrink.ofShrinker Shrink.signedNumber id)
            |> Arbitrary
        with get

    /// Generates int16 values that are between -size and size.
    static member val Int16 :Arbitrary<int16> =
            Gen.sized (fun s -> 
                Gen.choose(-s, s) |> Gen.map (int16 >> Shrink.ofShrinker Shrink.signedNumber id))
            |> Arbitrary
        with get

    /// Generates int16 values that are uniformly distributed in the whole range of int16 values.
    static member val DoNotSizeInt16 :Arbitrary<DoNotSize<int16>> = 
            Gen.choose(int Int16.MinValue, int Int16.MaxValue)
            |> Gen.map (int16 >> Shrink.ofShrinker Shrink.signedNumber DoNotSize)
            |> Arbitrary
        with get

    /// Generates uint16 values that are between 0 and size.
    static member val UInt16 :Arbitrary<uint16> =
            Gen.sized (fun s ->
                Gen.choose (0, s) |> Gen.map (uint16 >> Shrink.ofShrinker Shrink.unsignedNumber id))
            |> Arbitrary
        with get

    /// Generates uint16 values that are uniformly distributed in the whole range of uint16 values.
    static member val DoNotSizeUInt16 :Arbitrary<DoNotSize<uint16>> =
            Gen.choose(int UInt16.MinValue, int UInt16.MaxValue)
            |> Gen.map (uint16 >> Shrink.ofShrinker Shrink.unsignedNumber DoNotSize)
            |> Arbitrary
        with get
         
    /// Generates int32 values that are between -size and size.
    static member val Int32 :Arbitrary<int> =
            Arb.sized (fun n -> Arb.choose (-n,n))
        with get

    /// Generates int32 values that are uniformly distributed in the whole range of Int32 values.
    static member val DoNotSizeInt32 :Arbitrary<DoNotSize<int>> =
            Arb.choose(Int32.MinValue, Int32.MaxValue)
            |> Arb.map DoNotSize
        with get

    /// Generates uint32 values that are between 0 and size.
    static member val UInt32 :Arbitrary<uint32> =
            Gen.sized (fun s ->
                Gen.choose(0, s) |> Gen.map (uint32 >> Shrink.ofShrinker Shrink.unsignedNumber id))
            |> Arbitrary
        with get

    /// Generate arbitrary uint32 that is unrestricted by size.
    static member val DoNotSizeUInt32 :Arbitrary<DoNotSize<uint32>> =
            Gen.choose64(int64 UInt32.MinValue, int64 UInt32.MaxValue)
            |> Gen.map (uint32 >> Shrink.ofShrinker Shrink.unsignedNumber DoNotSize)
            |>Arbitrary
        with get

    ///Generates int64 values that are between -size and size.
    ///Note that since the size is an int32, this does not actually cover the full
    ///range of int64. See DoNotSize<int64> instead.
    static member val Int64 =
            Arb.sized(fun s -> Arb.choose64(int64 -s, int64 s))
        with get

    /// Generate arbitrary int64 that is unrestricted by size.
    static member val DoNotSizeInt64 : Arbitrary<DoNotSize<int64>> =
            Arb.choose64(Int64.MinValue, Int64.MaxValue)
            |> Arb.map DoNotSize
        with get
     
    /// Generates uint64 values that are between 0 and size.
    static member val UInt64 :Arbitrary<uint64> =
            Gen.sized (fun s -> 
                Gen.choose(0, s) |> Gen.map (uint64 >> Shrink.ofShrinker Shrink.unsignedNumber id))
            |> Arbitrary
        with get
     
    ///Generates uint64 values that are unrestricted by size.
    static member val DoNotSizeUInt64 :Arbitrary<DoNotSize<uint64>> =
            Gen.choose64(Int64.MinValue, Int64.MaxValue)
            |> Gen.map (uint64 >> Shrink.ofShrinker Shrink.unsignedNumber DoNotSize)
            |> Arbitrary
        with get

    ///Generates float values that are between -size and size (without NaN, Infinity, Epsilon, MinValue, MaxValue)
    ///Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val NormalFloat :Arbitrary<NormalFloat> =
            Arb.double
            |> Arb.sized 
            |> Arb.map NormalFloat
        with get

    /// Generates float values that are between -size and size.
    /// One of NaN, NegativeInfinity, PositiveInfinity, MaxValue, MinValue or Epsilon is generated on average 1 time for every 8 values.
    /// Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val Float :Arbitrary<float> = 
            Arb.frequency [(7, Arb.sized Arb.double)
                           (1, Arb.elements [ Double.NaN; Double.NegativeInfinity; Double.PositiveInfinity
                                              Double.MaxValue; Double.MinValue; Double.Epsilon])]

        with get

    /// Generates float values that are between -size and size.
    /// One of NaN, NegativeInfinity, PositiveInfinity, MaxValue, MinValue or Epsilon is generated on average 1 time for every 8 values.
    /// Shrinks by yielding zero, abs of the origin and the truncated origin.
    static member val Float32 :Arbitrary<float32> = 
            Arb.frequency [(7, Default.NormalFloat |> Arb.map (fun (NormalFloat f) -> float32 f))
                           (1, Arb.elements [ Single.NaN; Single.NegativeInfinity; Single.PositiveInfinity
                                              Single.MaxValue; Single.MinValue; Single.Epsilon])]
        with get

    static member val Complex :Arbitrary<Numerics.Complex> =
            Arb.map2 (fun real imaginary -> Numerics.Complex(real, imaginary)) Default.Float Default.Float
        with get

    /// Generates decimal values that are between -size and size.
    static member val Decimal :Arbitrary<decimal> =
            let decimalGen =
                let tenPow9 = 1_000_000_000
                Gen.choose(0, tenPow9 - 1)
                |> Gen.map Decimal
                |> Gen.map (fun d -> d / Decimal tenPow9)
            let generator =
                Gen.sized (fun size ->
                    Gen.map2
                        (fun d isNegative ->
                            let value = d * Decimal size
                            if isNegative then -value else value)
                        decimalGen
                        (Arb.toGen Default.Bool))
            let shrinker d =
                let (|<|) x y = abs x < abs y
                seq {
                    if d <> 0m then yield 0m
                    if d < 0m then yield -d
                    let truncated = truncate d
                    if truncated |<| d then yield truncated
                }
            Arb.fromGenShrink (generator, shrinker)
        with get

    /// Generates decimal values unrestricted by size.
    static member val DoNotSizeDecimal =
            let int32Gen =
                Default.DoNotSizeInt32
                |> Arb.toGen
                |> Gen.map (fun (DoNotSize i) -> i)
            let generator =
                Gen.map5
                    (fun lo mid hi isNegative scale -> Decimal(lo, mid, hi, isNegative, scale))
                    int32Gen
                    int32Gen
                    int32Gen
                    (Arb.toGen Default.Bool)
                    (Gen.choose(0, 28) |> Gen.map byte)
            let shrinker d =
                let (|<|) x y = abs x < abs y
                seq {
                    if d <> 0m then yield 0m
                    if d < 0m then yield -d
                    let truncated = truncate d
                    if truncated |<| d then yield truncated
                }
            Arb.fromGenShrink (generator, shrinker)
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get

    /////Generates complex values of form {float + i*float}. 
    /////Shrinks by removing the imaginary part and shrinking both parts.
    //static member val Complex =
    //        let arbFloat = Default.Float
    //        let arbFloat2 = Arb.zip (arbFloat, arbFloat)
    //        let generator = arbFloat2.Generator
    //                        |> Gen.map Numerics.Complex
    //        let shrinker (c : Numerics.Complex) =
    //            match (c.Real, c.Imaginary) with
    //            | (r, 0.0) -> 
    //                arbFloat.Shrinker r
    //                |> Seq.map (fun r -> Numerics.Complex(r, 0.0))
    //            | (r, i) ->
    //                let realOnly = seq { yield Numerics.Complex(r, 0.0)}
    //                let shrunk =
    //                    arbFloat2.Shrinker (r, i)
    //                    |> Seq.filter (fun (sr, si) -> not (si = 0.0 && sr.Equals r)) //We use Equals to properly compare NaNs
    //                    |> Seq.map Numerics.Complex
    //                Seq.append realOnly shrunk
    //        Arb.fromGenShrink (generator, shrinker)
    //    with get

    /// Generates characters that are between ASCII codes Char.MinValue and 127.
    static member val Char :Arbitrary<char> =
            Arb.choose (int Char.MinValue, 127) 
            |> Arb.map char
        with get

    ///Generates strings, which are lists of characters or null (1/10 of the time).
    static member val String :Arbitrary<string> =
            let nonNullString = Arb.array Default.Char |> Arb.map String
            Arb.frequency [(1, Arb.constant null); (9, nonNullString)]
        with get

    static member val UnicodeChar :Arbitrary<UnicodeChar> =
            let generator =
                Gen.choose (int Char.MinValue, int Char.MaxValue)
                |> Gen.map (char >> UnicodeChar)
            let shrinker (UnicodeChar c) =
                seq {
                    for candidate in [ 'a'; 'b'; 'c' ] do
                        if candidate < c || not (Char.IsLower c) then
                            yield UnicodeChar candidate
                }
            Arb.fromGenShrink (generator, shrinker)
        with get

    static member val UnicodeString :Arbitrary<UnicodeString> =
            Default.UnicodeChar
            |> Arb.map (fun (UnicodeChar c) -> c)
            |> Arb.array
            |> Arb.map String
            |> Arb.map UnicodeString
        with get

    /// Generates one-dimensional (rank 1) arrays. 
    /// The length of the generated array is between 0 and the test size. 
    /// The sum of the sizes of the elements is equal to the size of the generated array.
    static member Array(elements: Arbitrary<'T>) =
        elements
        |> Arb.array

    static member FixedLengthArray(elements: Arbitrary<'T>) =
        let generator =
            Gen.sized (fun size ->
                Gen.choose (0, size)
                |> Gen.bind (fun length ->
                    Arb.sequenceToArray (Seq.replicate length elements)
                    |> Arb.unArb))
        Arb.fromGenTree generator
        |> Arb.map FixedLengthArray
        
    ///Generates option values that are 'None' 1/8 of the time.
    static member Option(value: Arbitrary<'T>) = 
        value
        |> Arb.option

    ///Generates underlying values that are not null.
    static member NonNull(value: Arbitrary<'T>) =
        let inline notNull x = not (isNull x)
        value
        |> Arb.filter notNull
        |> Arb.map NonNull

    static member DoNotShrink(value: Arbitrary<'T>) =
        value
        |> Arb.toGen
        |> Gen.map DoNotShrink
        |> Arb.fromGen


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
        Arb.oneof
            [ Default.Char |> Arb.map box
              Default.String |> Arb.map box
              Default.Bool |> Arb.map box ]
        with get



    ///Generate a rank 2, zero based array. 
    ///The product of the width and the height is between 0 and the test size.
    // {NOT YET} The sum of the sizes of the elements is equal to the size of the generated array.
    static member Array2D(elements: Arbitrary<'T>) =
        let shrinkArray (array: 'T[,]) =
            seq {
                for row in 0 .. Array2D.length1 array - 1 do
                    yield Array2D.init (Array2D.length1 array - 1) (Array2D.length2 array) (fun i j -> array.[(if i < row then i else i + 1), j])
                for col in 0 .. Array2D.length2 array - 1 do
                    yield Array2D.init (Array2D.length1 array) (Array2D.length2 array - 1) (fun i j -> array.[i, (if j < col then j else j + 1)])
            }
        let generator = Gen.array2DOf (Arb.toGen elements)
        Arb.fromGenShrink (generator, shrinkArray)


    // NOTE: Function types are a bit special in that they need to have explicit type parameters.
    // Otherwise the discovery of the default arbitrary map will not work correctly, as it assumes
    // that if looking for a Arbitrary<FSharpFunc<int,string>> it can instantiate the generic method with
    // int and string in that order. Due to the "target" parameter however, the order of the generic parameters
    // is reversed if not given explicitly, so you end with a function from target to domain.

    /// Generates F# function values. Function values can be generated for types 'T->'U where the target domain 'U has an Arbitrary instance.
    /// There is no shrinking for function values.
    [<CompiledName("FSharpFunc")>]
    static member FSharpFunc<'T,'U when 'T:equality>(target: Arbitrary<'U>) : Arbitrary<'T -> 'U> =
        Arb.pureFunction target

    ///Generates F# function values that generate an instance of the function result type about half the time. The other 
    ///times it generate one of a list of common .NET exceptions, including Exception, ArgumentException, ArithmeticException,
    ///IOException, NotImplementedException, OUtOfMemoryException and others.
    [<CompiledName("ThrowingFSharpFunc")>]
    static member ThrowingFSharpFunc<'T,'U when 'T:equality>(target: Arbitrary<'U>) :Arbitrary<ThrowingFunction<'T,'U>> =
        let exceptions: exn array =
            [| Exception()
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
               IO.PathTooLongException() |]
        Arb.throwingFunction exceptions target
        |> Arb.map ThrowingFunction
   ///Generates Function values that can be printed and shrunk. Function values can be generated for types 'T->'U
    ///where 'b has an Arbitrary instance.
    static member Function<'T,'U when 'T:equality and 'T:comparison>(target: Arbitrary<'U>) :Arbitrary<Function<'T,'U>> =
        target
        |> Arb.pureFunction
        |> Arb.map Function<_,_>.From

    static member SystemFunc<'U>(target: Arbitrary<'U>) : Arbitrary<Func<'U>> =
        Default.FSharpFunc target
        |> Arb.convert (fun (f: unit -> 'U) -> Func<'U>(f)) (fun (f: Func<'U>) -> f.Invoke())

    static member SystemFunc1<'T,'U when 'T:equality>(target: Arbitrary<'U>) : Arbitrary<Func<'T,'U>> =
        target
        |> Default.FSharpFunc
        |> Arb.convert (fun (f: 'T -> 'U) -> Func<'T,'U>(f)) (fun (f: Func<'T,'U>) -> f.Invoke)

    static member SystemFunc2<'T1,'T2,'U when 'T1:equality and 'T2:equality>(target: Arbitrary<'U>) : Arbitrary<Func<'T1,'T2,'U>> =
        target
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun (f: 'T1 -> 'T2 -> 'U) -> Func<'T1,'T2,'U>(fun a b -> f a b)) (fun (f: Func<'T1,'T2,'U>) -> f.Invoke)

    static member SystemFunc3<'T1,'T2,'T3,'U when 'T1:equality and 'T2:equality and 'T3:equality>(target: Arbitrary<'U>) : Arbitrary<Func<'T1,'T2,'T3,'U>> =
        target
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun (f: 'T1 -> 'T2 -> 'T3 -> 'U) -> Func<'T1,'T2,'T3,'U>(fun a b c -> f a b c)) (fun (f: Func<'T1,'T2,'T3,'U>) -> f.Invoke)

    static member SystemAction() : Arbitrary<Action> =
        Default.Unit
        |> Default.FSharpFunc
        |> Arb.convert (fun (f: unit -> unit) -> Action(f)) (fun (f: Action) -> f.Invoke())

    static member SystemAction1<'T when 'T:equality>() : Arbitrary<Action<'T>> =
        Default.Unit
        |> Default.FSharpFunc
        |> Arb.convert (fun (f: 'T -> unit) -> Action<'T>(f)) (fun (f: Action<'T>) -> f.Invoke)

    static member SystemAction2<'T1,'T2 when 'T1:equality and 'T2:equality>() : Arbitrary<Action<'T1,'T2>> =
        Default.Unit
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun (f: 'T1 -> 'T2 -> unit) -> Action<'T1,'T2>(fun a b -> f a b)) (fun (f: Action<'T1,'T2>) -> f.Invoke)

    static member SystemAction3<'T1,'T2,'T3 when 'T1:equality and 'T2:equality and 'T3:equality>() : Arbitrary<Action<'T1,'T2,'T3>> =
        Default.Unit
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Default.FSharpFunc
        |> Arb.convert (fun (f: 'T1 -> 'T2 -> 'T3 -> unit) -> Action<'T1,'T2,'T3>(fun a b c -> f a b c)) (fun (f: Action<'T1,'T2,'T3>) -> f.Invoke)

    //// NOTE: Function types are a bit special in that they need to have explicit type parameters.
    //// Otherwise the discovery of the default arbitrary map will not work correctly, as it assumes
    //// that if looking for a Arbitrary<FSharpFunc<int,string>> it can instantiate the generic method with
    //// int and string in that order. Due to the "target" parameter however, the order of the generic parameters
    //// is reversed if not given explicitly, so you end with a function from target to domain.

    ///// Generates F# function values. Function values can be generated for types 'T->'U where the target domain 'U has an Arbitrary instance.
    ///// There is no shrinking for function values.
    //[<CompiledName("FSharpFunc")>]
    //static member FSharpFunc<'T,'U when 'T:equality>(target: Arbitrary<'U>) :Arbitrary<'T->'U> = 
    //    target.Generator
    //    |> Gen.pureFunction
    //    |> Arb.fromGen

    /////Generates F# function values that generate an instance of the function result type about half the time. The other 
    /////times it generate one of a list of common .NET exceptions, including Exception, ArgumentException, ArithmeticException,
    /////IOException, NotImplementedException, OUtOfMemoryException and others.
    //[<CompiledName("ThrowingFSharpFunc")>]
    //static member ThrowingFSharpFunc<'T,'U when 'T:equality>(target: Arbitrary<'U>) :Arbitrary<ThrowingFunction<'T,'U>> = 
    //    let exns: exn array = [|Exception()

    //                            ArgumentException()
    //                            ArgumentNullException()
    //                            ArgumentOutOfRangeException()
                                     
    //                            ArithmeticException()
    //                            DivideByZeroException()
    //                            OverflowException()
    //                            FormatException()
    //                            IndexOutOfRangeException()
    //                            InvalidCastException()
    //                            InvalidOperationException()
                                     
    //                            IO.IOException()
    //                            IO.EndOfStreamException()
    //                            IO.FileNotFoundException()

    //                            NotImplementedException()
    //                            NotSupportedException()

    //                            NullReferenceException()
    //                            OutOfMemoryException()

    //                            NotFiniteNumberException()
    //                            StackOverflowException()

    //                            IO.DirectoryNotFoundException()
    //                            IO.FileLoadException()
    //                            KeyNotFoundException()
    //                            IO.PathTooLongException()
    //                        |]
    //    target |> Arb.throwingFunction exns |> Arb.convert ThrowingFunction (fun tf -> tf.Get)

    /////Generates Function values that can be printed and shrunk. Function values can be generated for types 'T->'U
    /////where 'b has an Arbitrary instance.
    //static member Function<'T,'U when 'T:equality and 'T:comparison>(target: Arbitrary<'U>) : Arbitrary<Function<'T,'U>> =
    //    let generator =
    //        target.Generator
    //        |> Gen.pureFunction 
    //        |> Gen.map Function<_,_>.From
    //    let shrinker (f:Function<_, _>) = 
    //        let update x' y' f x = if x = x' then y' else f x
    //        seq { for (x,y) in f.Table do 
    //                for y' in target.Shrinker y do 
    //                    yield Function<_,_>.From (update x y' f.Value) }
    //    Arb.fromGenShrink(generator, shrinker)

    /////Generates Func'1 values.
    //static member SystemFunc(target: Arbitrary<'U>) =
    //    Default.FSharpFunc(target)
    //    |> Arb.convert (fun f -> Func<_>(f)) (fun f -> f.Invoke)

    /////Generates Func'2 values.
    //static member SystemFunc1(target: Arbitrary<'U>) =
    //    target
    //    |> Default.FSharpFunc
    //    |> Arb.convert (fun f -> Func<_,_>(f)) (fun f -> f.Invoke)

    /////Generates Func'3 values.
    //static member SystemFunc2(target: Arbitrary<'U>) =
    //    target
    //    |> Default.FSharpFunc
    //    |> Default.FSharpFunc
    //    |> Arb.convert (fun f -> Func<_,_,_>(f)) (fun f a b -> f.Invoke(a,b))

    /////Generates Func'4 values.
    //static member SystemFunc3(target: Arbitrary<'U>) =
    //    target
    //    |> Default.FSharpFunc
    //    |> Default.FSharpFunc
    //    |> Default.FSharpFunc
    //    |> Arb.convert (fun f -> Func<_,_,_,_>(f)) (fun f a b c -> f.Invoke(a,b,c))

    /////Generates Action'0 values.
    //static member SystemAction() =
    //    Default.Unit
    //    |> Default.FSharpFunc
    //    |> Arb.convert (fun f -> Action(f)) (fun f -> f.Invoke)

    /////Generates Action'1 values.
    //static member SystemAction1() =
    //    Default.Unit
    //    |> Default.FSharpFunc
    //    |> Arb.convert (fun f -> Action<_>(f)) (fun f -> f.Invoke)

    /////Generates Action'2 values.
    //static member SystemAction2() =
    //    Default.Unit
    //    |> Default.FSharpFunc
    //    |> Default.FSharpFunc
    //    |> Arb.convert (fun f -> Action<_,_>(f)) (fun f a b -> f.Invoke(a,b))

    /////Generates Action'3 values.
    //static member SystemAction3() =
    //    Default.Unit
    //    |> Default.FSharpFunc
    //    |> Default.FSharpFunc
    //    |> Default.FSharpFunc
    //    |> Arb.convert (fun f -> Action<_,_,_>(f)) (fun f a b c -> f.Invoke(a,b,c))

    /// Generates DateTime values between 1900 and 2100.
    static member val DateTime :Arbitrary<DateTime> =
            let genDate =
                gen {
                    let! y = Gen.choose(1900, 2100)
                    and! m = Gen.choose(1, 12)
                    let! d = Gen.choose(1, DateTime.DaysInMonth(y, m))
                    and! h = Gen.choose(0, 23)
                    and! min = Gen.choose(0, 59)
                    and! sec = Gen.choose(0, 59)
                    and! ms = Gen.choose(0, 999)
                    and! kind = Gen.elements [ DateTimeKind.Unspecified; DateTimeKind.Utc; DateTimeKind.Local ]
                    return DateTime(y, m, d, h, min, sec, ms, kind)
                }
            Arb.fromGenShrink (genDate, Shrink.date)
        with get

    static member val DoNotSizeDateTime :Arbitrary<DoNotSize<DateTime>> =
            let generator =
                gen {
                    let! (DoNotSize ticks) = Arb.toGen Default.DoNotSizeInt64
                    and! kind = Gen.elements [ DateTimeKind.Unspecified; DateTimeKind.Utc; DateTimeKind.Local ]
                    return DateTime(abs ticks, kind)
                }
            Arb.fromGenShrink (generator, Shrink.date)
            |> Arb.convert DoNotSize DoNotSize.Unwrap
        with get

    static member val TimeSpan :Arbitrary<TimeSpan> =
            let generator =
                Default.DoNotSizeInt64
                |> Arb.toGen
                |> Gen.map (fun (DoNotSize ticks) -> TimeSpan ticks)
            let shrink (timeSpan: TimeSpan) =
                if timeSpan.Days <> 0 then
                    seq { yield TimeSpan(0, timeSpan.Hours, timeSpan.Minutes, timeSpan.Seconds, timeSpan.Milliseconds) }
                elif timeSpan.Hours <> 0 then
                    seq { yield TimeSpan(0, 0, timeSpan.Minutes, timeSpan.Seconds, timeSpan.Milliseconds) }
                elif timeSpan.Minutes <> 0 then
                    seq { yield TimeSpan(0, 0, 0, timeSpan.Seconds, timeSpan.Milliseconds) }
                elif timeSpan.Seconds <> 0 then
                    seq { yield TimeSpan(0, 0, 0, 0, timeSpan.Milliseconds) }
                elif timeSpan.Milliseconds <> 0 then
                    seq { yield TimeSpan.Zero }
                else
                    Seq.empty
            Arb.fromGenShrink (generator, shrink)
        with get

    static member val DateTimeOffset :Arbitrary<DateTimeOffset> =
            let genTimeZone =
                gen {
                    let! hours = Gen.choose(-12, 14)
                    let! minutes =
                        if hours = -12 || hours = 14 then Gen.constant 0
                        else Gen.choose(0, 59)
                    return TimeSpan(hours, minutes, 0)
                }
            let generator =
                Gen.map2
                    (fun date offset ->
                        DateTimeOffset(DateTime.SpecifyKind(date, DateTimeKind.Unspecified), offset))
                    (Arb.toGen Default.DateTime)
                    genTimeZone
            let shrinkOffset (offset: TimeSpan) =
                seq {
                    if offset.Hours <> 0 then
                        yield TimeSpan(0, 0, offset.Minutes, 0, 0)
                    if offset.Minutes <> 0 then
                        yield TimeSpan.Zero
                }
                |> Seq.where (fun shrunk -> shrunk.Hours > 0 || shrunk.Minutes > 0)
            let shrink (date: DateTimeOffset) =
                seq {
                    yield! shrinkOffset date.Offset |> Seq.map (fun offset -> DateTimeOffset(date.DateTime, offset))
                    if date.Offset <> TimeSpan.Zero then
                        yield DateTimeOffset(date.DateTime, TimeSpan.Zero)
                    yield! Shrink.date date.DateTime |> Seq.map (fun dateTime -> DateTimeOffset(dateTime, TimeSpan.Zero))
                }
            Arb.fromGenShrink (generator, shrink)
        with get

    static member KeyValuePair(key: Arbitrary<'K>, value: Arbitrary<'V>) =
        Arb.map2 (fun k v -> KeyValuePair(k, v)) key value

    /////Generates DateTime values that are unrestricted by size.
    /////A DateTime is shrunk by removing its Kind, millisecond, second, minute and hour components.
    //static member val DoNotSizeDateTime =
    //        let genDate = gen { let! (DoNotSize ticks) = Default.DoNotSizeInt64.Generator
    //                            and! kind = Gen.elements [DateTimeKind.Unspecified; DateTimeKind.Utc; DateTimeKind.Local]
    //                            return DateTime(abs ticks, kind) }
    //        Arb.fromGenShrink (genDate, Shrink.date)
    //        |> Arb.convert DoNotSize DoNotSize.Unwrap
    //    with get

    /////Generates TimeSpan values that are unrestricted by size. 
    /////A TimeSpan is shrunk by removing days, hours, minutes, second and milliseconds.
    //static member val TimeSpan :Arbitrary<TimeSpan> =
    //        let genTimeSpan = Default.DoNotSizeInt64.Generator |> Gen.map (fun (DoNotSize i) -> TimeSpan i)
    //        let shrink (t: TimeSpan) = 
    //            if t.Days <> 0 then
    //                seq { yield TimeSpan(0, t.Hours, t.Minutes, t.Seconds, t.Milliseconds) }
    //            elif t.Hours <> 0 then
    //                seq { yield TimeSpan(0, 0, t.Minutes, t.Seconds, t.Milliseconds) }
    //            elif t.Minutes <> 0 then
    //                seq { yield TimeSpan(0, 0, 0, t.Seconds, t.Milliseconds) }
    //            elif t.Seconds <> 0 then
    //                seq { yield TimeSpan(0, 0, 0, 0, t.Milliseconds) }
    //            elif t.Milliseconds <> 0 then
    //                seq { yield TimeSpan.Zero }
    //            else
    //                Seq.empty
    //        Arb.fromGenShrink (genTimeSpan, shrink)
    //    with get

    /////Generates DateTimeOffset values that are between 1900 and 2100. 
    ///// A DateTimeOffset is shrunk first by shrinking its offset, then by removing its second, minute and hour components.
    //static member val DateTimeOffset =
    //        let genTimeZone = gen {
    //            let! hours = Gen.choose(-12, 14)
    //            let! minutes =
    //                if hours = -12 || hours = 14 then
    //                    Gen.constant 0
    //                else 
    //                    Gen.choose(0, 59)
    //            return TimeSpan(hours, minutes, 0) }
    //        let arbTimeSpan = Default.TimeSpan
    //        let shrinkTimeZone (t: TimeSpan) =
    //            arbTimeSpan.Shrinker t |> Seq.where (fun z -> z.Hours > 0 || z.Minutes > 0)
    //        let genDateTime = Default.DateTime.Generator
    //        let genDate = gen { 
    //            let! t = genDateTime
    //            and! tz = genTimeZone
    //            return DateTimeOffset(DateTime.SpecifyKind(t, DateTimeKind.Unspecified), tz) }
    //        let shrinkDateTimeOffset (d: DateTimeOffset) =
    //            seq {
    //                for ts in shrinkTimeZone d.Offset ->
    //                    DateTimeOffset(d.DateTime, ts)
    //                if d.Offset <> TimeSpan.Zero then
    //                    yield DateTimeOffset(d.DateTime, TimeSpan.Zero)
    //                for dt in Default.DateTime.Shrinker d.DateTime ->
    //                    DateTimeOffset(dt, TimeSpan.Zero) }
    //        Arb.fromGenShrink (genDate, shrinkDateTimeOffset)
    //    with get

    //static member KeyValuePair(key: Arbitrary<'K>, value: Arbitrary<'V>) =
    //    let genKeyValuePair =
    //        Gen.map2 (fun k v -> KeyValuePair(k, v)) key.Generator value.Generator
    //    let shrinkKeyValuePair (kvp:KeyValuePair<_,_>) = 
    //        seq { for key in key.Shrinker kvp.Key do
    //                for value in value.Shrinker kvp.Value do
    //                    yield new KeyValuePair<_,_>(key, value) }
    //    Arb.fromGenShrink(genKeyValuePair,shrinkKeyValuePair)

    static member val NegativeInt =
            Default.Int32
            |> Arb.map (fun x -> -abs x) // fsharplint:disable-line CanBeReplacedWithComposition
            |> Arb.filter(fun x -> x < 0)
            |> Arb.map NegativeInt
        with get

    static member val NonNegativeInt =
            Default.Int32
            |> Arb.map (abs >> NonNegativeInt)
        with get

    static member val PositiveInt =
            Default.Int32
            |> Arb.map abs
            |> Arb.filter (fun i -> i > 0)
            |> Arb.map PositiveInt
        with get

    static member val NonZeroInt =
            Default.Int32
            |> Arb.filter ((<>) 0)
            |> Arb.map NonZeroInt
        with get

    static member val Interval :Arbitrary<Interval> =
            Arb.map2 (fun start offset -> Interval(abs start, abs start + abs offset)) Default.Int32 Default.Int32
        with get

    static member val IntWithMinMax :Arbitrary<IntWithMinMax> =
            Arb.frequency
                [ 1, Arb.elements [ Int32.MaxValue; Int32.MinValue ]
                  10, Default.Int32 ]
            |> Arb.map IntWithMinMax
        with get

    //static member val IntWithMinMax =
    //        let arbInt32 = Default.Int32
    //        let generator = Gen.frequency [(1,  Gen.elements [Int32.MaxValue; Int32.MinValue])
    //                                       (10, arbInt32.Generator) ] 
    //                        |> Gen.map IntWithMinMax
    //        let shrinker (IntWithMinMax i) = arbInt32.Shrinker i |> Seq.map IntWithMinMax
    //        Arb.fromGenShrink(generator, shrinker)
    //    with get

    /////Generates intervals between two non-negative integers.
    //static member val Interval =
    //        let arbInt32 = Default.Int32
    //        let arbTuple = Arb.zip(arbInt32, arbInt32)
    //        let generator = 
    //            arbInt32.Generator
    //            |> Gen.two
    //            |> Gen.map (fun (start, offset) -> Interval (abs start, abs start + abs offset))
    //        let shrinker (i : Interval) =
    //            (i.Left, i.Right - i.Left)
    //            |> arbTuple.Shrinker
    //            |> Seq.map (fun (start, offset) -> Interval(start, start + offset))
    //        Arb.fromGenShrink(generator, shrinker)
    //    with get

    static member val StringNoNullChar :Arbitrary<StringNoNullChar> =
            Default.String
            |> Arb.filter (not << String.exists ((=) '\000'))
            |> Arb.map StringNoNullChar
        with get

    static member val NonEmptyString :Arbitrary<NonEmptyString> =
            Default.String
            |> Arb.filter (fun s -> not (String.IsNullOrEmpty s) && not (String.exists ((=) '\000') s))
            |> Arb.map NonEmptyString
        with get

    static member val NonWhiteSpaceString :Arbitrary<NonWhiteSpaceString> =
            Default.String
            |> Arb.filter (fun s -> not (String.IsNullOrWhiteSpace s) && not (String.exists ((=) '\000') s))
            |> Arb.map NonWhiteSpaceString
        with get

    static member val XmlEncodedString :Arbitrary<XmlEncodedString> =
            Default.String
            |> Arb.map System.Net.WebUtility.HtmlEncode
            |> Arb.filter (String.forall Xml.XmlConvert.IsXmlChar)
            |> Arb.map XmlEncodedString
        with get

    static member Set(elements: Arbitrary<'T>) = 
        elements
        |> Arb.set

    static member Map(keys: Arbitrary<'K>, values: Arbitrary<'V>) = 
        Arb.mapKV (keys, values)

    static member NonEmptyArray(elements: Arbitrary<'T>) =
        Default.Array(elements)
        |> Arb.filter (fun a -> Array.length a > 0)
        |> Arb.map NonEmptyArray

    static member NonEmptySet(elements: Arbitrary<'T>) =
        Default.Set(elements)
        |> Arb.filter (not << Set.isEmpty) 
        |> Arb.map NonEmptySet

    /////Arrays whose length does not change when shrinking.
    //static member FixedLengthArray(elements: Arbitrary<'T>) =
    //    let arbArray = Default.Array(elements)
    //    let generator = arbArray.Generator
    //    Arb.fromGenShrink (generator, Shrink.arrayElements elements.Shrinker)
    //    |> Arb.convert FixedLengthArray (fun (FixedLengthArray a) -> a)

    /// Generates System.Collections.Generic.List instances.
    static member List(elements: Arbitrary<'T>) =
        Default.Array(elements)
        |> Arb.map Enumerable.ToList

    /// Generates System.Collections.Generic.IList instances.
    static member IList(elements: Arbitrary<'T>) =
        Default.List(elements)
        |> Arb.map (fun x -> x :> _ IList)

    /// Generates System.Collections.Generic.ICollection instances.
    static member ICollection(elements: Arbitrary<'T>) =
        Default.List(elements)
        |> Arb.map (fun x -> x :> _ ICollection)

    /// Generates System.Collections.Generic.Dictionary instances.
    static member Dictionary(keys: Arbitrary<'K>, values: Arbitrary<'V>) :Arbitrary<Dictionary<'K,'V>>=
        let keyValue =
            Arb.map2 (fun key value -> key, value)
                (keys |> Arb.filter (fun key -> not (isNull (box key))))
                values
        let generator =
            keyValue
            |> Arb.array
            |> Arb.toGen
            |> Gen.map (fun entries ->
                let dictionary = Dictionary<'K, 'V>()
                for key, value in entries do
                    if not (dictionary.ContainsKey key) then
                        dictionary.Add(key, value)
                dictionary)
        let shrinkDictionary (dictionary: Dictionary<'K, 'V>) =
            let entries = dictionary |> Seq.toArray
            seq {
                for count in entries.Length - 1 .. -1 .. 0 do
                    let shrunk = Dictionary<'K, 'V>()
                    for index in 0 .. count - 1 do
                        let entry = entries.[index]
                        shrunk.Add(entry.Key, entry.Value)
                    yield shrunk
            }
        Arb.fromGenShrink (generator, shrinkDictionary)

    /// Generates System.Collections.Generic.IDictionary instances.
    static member IDictionary(keys: Arbitrary<'K>, values: Arbitrary<'V>) =
        Default.Dictionary(keys, values)
        |> Arb.map (fun x -> x :> IDictionary<_,_>)

    static member val Culture :Arbitrary<CultureInfo> =
            let generator =
                Gen.elements (CultureInfo.GetCultures(CultureTypes.NeutralCultures ||| CultureTypes.SpecificCultures))
            let shrink culture =
                Seq.unfold
                    (fun current ->
                        if isNull current || current = CultureInfo.InvariantCulture || isNull current.Parent then None
                        else Some(current.Parent, current.Parent))
                    culture
            Arb.fromGenShrink (generator, shrink)
        with get

    static member val Guid :Arbitrary<Guid> =
            let int32Generator =
                Default.DoNotSizeInt32
                |> Arb.toGen
                |> Gen.map (fun (DoNotSize value) -> value)
            let int16Generator =
                Default.DoNotSizeInt16
                |> Arb.toGen
                |> Gen.map (fun (DoNotSize value) -> value)
            let byteGenerator = Arb.toGen Default.Byte
            let generator =
                gen {
                    let! a = int32Generator
                    and! b = int16Generator
                    and! c = int16Generator
                    and! d = byteGenerator
                    and! e = byteGenerator
                    and! f = byteGenerator
                    and! g = byteGenerator
                    and! h = byteGenerator
                    and! i = byteGenerator
                    and! j = byteGenerator
                    and! k = byteGenerator
                    return Guid(a, b, c, d, e, f, g, h, i, j, k)
                }
            Arb.fromGen generator
        with get

    static member val ConsoleKeyInfo :Arbitrary<ConsoleKeyInfo> =
            let generator =
                gen {
                    let! character = Arb.toGen Default.Char
                    and! key = ReflectiveGenerator.enumOfType typeof<ConsoleKey> |> Arb.toGen
                    and! shift = Arb.toGen Default.Bool
                    and! alt = Arb.toGen Default.Bool
                    and! control = Arb.toGen Default.Bool
                    return ConsoleKeyInfo(character, unbox<ConsoleKey> key, shift, alt, control)
                }
            let shrink (info: ConsoleKeyInfo) =
                let hasModifier modifier = (info.Modifiers &&& modifier) <> enum<ConsoleModifiers> 0
                let shift, alt, control =
                    hasModifier ConsoleModifiers.Shift,
                    hasModifier ConsoleModifiers.Alt,
                    hasModifier ConsoleModifiers.Control
                seq {
                    if shift then yield false, alt, control
                    if alt then yield shift, false, control
                    if control then yield shift, alt, false
                }
                |> Seq.map (fun (shift, alt, control) ->
                    ConsoleKeyInfo(info.KeyChar, info.Key, shift, alt, control))
            Arb.fromGenShrink (generator, shrink)
        with get

    static member val IPv4Address :Arbitrary<IPv4Address> =
            let byteGenerator = Arb.toGen Default.Byte
            let generator =
                Gen.arrayOfLength 4 byteGenerator
                |> Gen.map (IPAddress >> IPv4Address)
            let shrink (IPv4Address address) =
                address.GetAddressBytes()
                |> Shrink.arrayElements (fun value -> Shrink.unsignedNumber value |> Seq.map byte)
                |> Seq.map (IPAddress >> IPv4Address)
            Arb.fromGenShrink (generator, shrink)
        with get

    static member val IPv6Address :Arbitrary<IPv6Address> =
            let byteGenerator = Arb.toGen Default.Byte
            let generator =
                Gen.arrayOfLength 16 byteGenerator
                |> Gen.map (IPAddress >> IPv6Address)
            let shrink (IPv6Address address) =
                address.GetAddressBytes()
                |> Shrink.arrayElements (fun value -> Shrink.unsignedNumber value |> Seq.map byte)
                |> Seq.map (IPAddress >> IPv6Address)
            Arb.fromGenShrink (generator, shrink)
        with get

    static member val IPAddress :Arbitrary<IPAddress> =
            let byteGenerator = Arb.toGen Default.Byte
            let generator =
                Gen.choose (0, 1)
                |> Gen.bind (fun length ->
                    Gen.arrayOfLength (if length = 0 then 4 else 16) byteGenerator)
                |> Gen.map IPAddress
            let shrink (address: IPAddress) =
                address.GetAddressBytes()
                |> Shrink.arrayElements (fun value -> Shrink.unsignedNumber value |> Seq.map byte)
                |> Seq.map IPAddress
            Arb.fromGenShrink (generator, shrink)
        with get

    static member val HostName :Arbitrary<HostName> =
            let isValidSubdomain (subdomain: string) =
                not (String.IsNullOrWhiteSpace subdomain)
                && subdomain.Length <= 63
                && not (subdomain.StartsWith("-"))
                && not (subdomain.EndsWith("-"))
            let isValidHost (host: string) =
                not (String.IsNullOrWhiteSpace host)
                && host.Length <= 255
                && not (host.StartsWith("."))
                && host.Split('.') |> Array.forall isValidSubdomain
            let subdomain =
                gen {
                    let characters = "abcdefghijklmnopqrstuvwxyz0123456789-".ToCharArray()
                    let! length = Gen.choose(1, 63)
                    return! Gen.elements characters
                            |> Gen.arrayOfLength length
                            |> Gen.map String
                            |> Gen.filter isValidSubdomain
                }
            let generator =
                gen {
                    let! tld = Gen.elements Data.topLevelDomains
                    let! count = Gen.frequency [ 20, Gen.constant 0; 4, Gen.constant 1; 2, Gen.constant 2; 1, Gen.constant 3 ]
                    return! Gen.listOfLength count subdomain
                            |> Gen.map (fun parts -> String.concat "." (parts @ [ tld ]))
                            |> Gen.filter isValidHost
                            |> Gen.map HostName
                }
            let shrink (HostName host) =
                let parts = host.Split('.')
                let tld = parts.[parts.Length - 1]
                seq {
                    if parts.Length > 1 then
                        yield String.concat "." parts.[1..]
                    if not (Seq.contains tld Data.commonTopLevelDomains) then
                        yield! Data.commonTopLevelDomains
                                |> Seq.map (fun commonTld ->
                                    Array.append parts.[0..parts.Length - 2] [| commonTld |]
                                    |> String.concat ".")
                }
                |> Seq.map HostName
            Arb.fromGenShrink (generator, shrink)
        with get

    static member val MailAddress :Arbitrary<MailAddress> =
            let isValidUser (user: string) =
                not (String.IsNullOrWhiteSpace user)
                && not (user.StartsWith("\""))
                && not (user.EndsWith("\""))
                && not (user.StartsWith("."))
                && not (user.EndsWith("."))
                && not (user.Contains(".."))
            let split (value: string) =
                if String.IsNullOrWhiteSpace value || value.Length <= 1 then Seq.empty
                else
                    seq {
                        yield value.[0 .. value.Length / 2 - 1]
                        yield value.[value.Length / 2 ..]
                    }
            let createMailAddress name user host =
                if String.IsNullOrWhiteSpace name then
                    MailAddress(sprintf "%s@%s" user host)
                else
                    MailAddress(sprintf "%s <%s@%s>" name user host)
            let name =
                gen {
                    let! length = Gen.choose(1, 63)
                    return! Gen.elements "abcdefghijklmnopqrstuvwxyz0123456789!#$%&'*+-/=?^_`.{|}~,:;[] "
                            |> Gen.arrayOfLength (length - 2)
                            |> Gen.map String
                }
            let user =
                gen {
                    let characters = "abcdefghijklmnopqrstuvwxyz0123456789!#$%&'*+-/=?^_`{|}~"
                    let! length = Gen.choose(1, 63)
                    return! Gen.elements characters
                            |> Gen.arrayOfLength length
                            |> Gen.map String
                            |> Gen.filter isValidUser
                }
            let generator =
                gen {
                    let! displayName = name
                    and! user = user
                    and! (HostName host) = Arb.toGen Default.HostName
                    return createMailAddress displayName user host
                }
            let shrink (address: MailAddress) =
                let shrinkDisplayName =
                    if String.IsNullOrWhiteSpace address.DisplayName then Seq.empty
                    else
                        seq {
                            yield! split address.DisplayName
                                  |> Seq.map (fun displayName -> createMailAddress displayName address.User address.Host)
                            yield createMailAddress "" address.User address.Host
                        }
                let shrinkUser =
                    split address.User
                    |> Seq.map (fun user -> createMailAddress address.DisplayName user address.Host)
                let shrinkHost (HostName host) =
                    let parts = host.Split('.')
                    seq {
                        if parts.Length > 1 then yield String.concat "." parts.[1..]
                        if not (Seq.contains parts.[parts.Length - 1] Data.commonTopLevelDomains) then
                            yield! Data.commonTopLevelDomains
                                    |> Seq.map (fun commonTld ->
                                        Array.append parts.[0..parts.Length - 2] [| commonTld |]
                                        |> String.concat ".")
                    }
                    |> Seq.map (fun host -> createMailAddress address.DisplayName address.User host)
                seq {
                    yield! shrinkDisplayName
                    yield! shrinkUser
                    yield! shrinkHost (HostName address.Host)
                }
                |> Seq.distinct
            Arb.fromGenShrink (generator, shrink)
        with get

    //static member val IPv4Address =
    //        let arbByte = Default.Byte
    //        let generator =
    //            arbByte.Generator
    //            |> Gen.arrayOfLength 4
    //            |> Gen.map (IPAddress >> IPv4Address)
    //        let shrinker (IPv4Address a) =
    //            a.GetAddressBytes()
    //            |> Shrink.arrayElements arbByte.Shrinker
    //            |> Seq.map (IPAddress >> IPv4Address)
     
    //        Arb.fromGenShrink (generator, shrinker)
    //    with get

    //static member val IPv6Address =
    //        let arbByte = Default.Byte
    //        let generator =
    //            arbByte.Generator
    //            |> Gen.arrayOfLength 16
    //            |> Gen.map (IPAddress >> IPv6Address)
    //        let shrinker (IPv6Address a) =
    //            a.GetAddressBytes()
    //            |> Shrink.arrayElements arbByte.Shrinker
    //            |> Seq.map (IPAddress >> IPv6Address)
     
    //        Arb.fromGenShrink (generator, shrinker)
    //    with get

    //static member val IPAddress =
    //        let arbByte = Default.Byte
    //        let generator = gen {
    //            let! byteLength = Gen.elements [4; 16]
    //            let! bytes = arbByte.Generator |> Gen.arrayOfLength byteLength
    //            return IPAddress bytes }
    //        let shrinker (a:IPAddress) =
    //            a.GetAddressBytes()
    //            |> Shrink.arrayElements arbByte.Shrinker
    //            |> Seq.map IPAddress
     
    //        Arb.fromGenShrink (generator, shrinker)
    //    with get

    //static member val HostName :Arbitrary<HostName> =
    //        let isValidSubdomain (subDomain: string) = String.IsNullOrWhiteSpace subDomain |> not && subDomain.Length <= 63 && subDomain.StartsWith("-") |> not && subDomain.EndsWith("-") |> not
    //        let isValidHost (host: string) = String.IsNullOrWhiteSpace host |> not && host.Length <= 255 && host.StartsWith(".") |> not && host.Split('.') |> Array.forall isValidSubdomain
    //        let subdomain = 
    //            gen {
    //                let subdomainCharacters = "abcdefghijklmnopqrstuvwxyz0123456789-".ToCharArray()
    //                let! subdomainLength = Gen.choose (1, 63)
    //                return! 
    //                    Gen.elements subdomainCharacters 
    //                    |> Gen.arrayOfLength subdomainLength 
    //                    |> Gen.map String
    //                    |> Gen.filter isValidSubdomain
    //            }

    //        let host = 
    //            gen {
    //                let! tld = Gen.elements Data.topLevelDomains
    //                let! numberOfSubdomains = Gen.frequency [(20, Gen.constant 0); (4, Gen.constant 1); (2, Gen.constant 2); (1, Gen.constant 3)]
             
    //                return! 
    //                    Gen.listOfLength numberOfSubdomains subdomain
    //                    |> Gen.map (fun x -> x @ [tld] |> String.concat ".")
    //                    |> Gen.filter isValidHost
    //                    |> Gen.map HostName
    //            }

    //        let shrinkHost (HostName host) =
    //            let parts = host.Split '.'
    //            let topLevelDomain = parts.[parts.Length - 1]

    //            seq {
    //                if parts.Length > 1 then
    //                    yield parts.[1 ..] |> String.concat "."
    //                if Seq.exists (fun tld -> tld = topLevelDomain) Data.commonTopLevelDomains |> not then
    //                    yield! Data.commonTopLevelDomains
    //                            |> Seq.map (fun tld -> Array.append parts.[0 .. parts.Length - 2] [|tld|] |> String.concat ".") }
    //            |> Seq.map HostName

    //        Arb.fromGenShrink (host, shrinkHost)
    //    with get

    //static member val MailAddress =
    //        let isValidUser (user: string) = 
    //            String.IsNullOrWhiteSpace user |> not &&
    //            not (user.StartsWith("\"")) && 
    //            not (user.EndsWith("\"")) && 
    //            not (user.StartsWith(".")) && 
    //            not (user.EndsWith(".")) && 
    //            not (user.Contains(".."))

    //        let split (str: string) = 
    //            if String.IsNullOrWhiteSpace str || str.Length <= 1 then 
    //                Seq.empty
    //            else seq {
    //                yield str.[0 .. str.Length / 2 - 1]
    //                yield str.[str.Length / 2 ..]
    //            }

    //        let createMailAddress name user host =
    //            if String.IsNullOrWhiteSpace name then
    //                MailAddress (sprintf "%s@%s" user host)
    //            else 
    //                MailAddress (sprintf "%s <%s@%s>" name user host)      

    //        let name = 
    //            gen {
    //                let! localLength = Gen.choose (1, 63)

    //                return! Gen.elements "abcdefghijklmnopqrstuvwxyz0123456789!#$%&'*+-/=?^_`.{|}~,:;[] "
    //                        |> Gen.arrayOfLength (localLength - 2) 
    //                        |> Gen.map String
    //            }

    //        let host = 
    //            Default.HostName.Generator
    //            |> Gen.map (fun (HostName h) -> h)

    //        let user = 
    //            gen {
    //                let userChars = "abcdefghijklmnopqrstuvwxyz0123456789!#$%&'*+-/=?^_`{|}~"
    //                let! localLength = Gen.choose (1, 63)

    //                return! Gen.elements userChars
    //                        |> Gen.arrayOfLength localLength
    //                        |> Gen.map String
    //                        |> Gen.filter isValidUser
    //            }
         
    //        let generator = 
    //            gen {
    //                let! name = name
    //                let! user = user
    //                let! host = host
    //                return createMailAddress name user host
    //            }

    //        let shrinkDisplayName (a:MailAddress) =
    //            if String.IsNullOrWhiteSpace a.DisplayName then
    //                Seq.empty
    //            else seq {
    //                yield! a.DisplayName |> split |> Seq.map (fun displayName -> createMailAddress displayName a.User a.Host)
    //                yield createMailAddress "" a.User a.Host
    //            }

    //        let shrinkUser (a:MailAddress) = 
    //            a.User |> split |> Seq.map (fun user -> createMailAddress a.DisplayName user a.Host)

    //        let shrinkHost (a:MailAddress) = 
    //            Default.HostName.Shrinker (HostName a.Host)
    //            |> Seq.map (fun (HostName h) -> createMailAddress a.DisplayName a.User h)
         
    //        let shrinker (a:MailAddress) = 
    //            seq {
    //                yield! shrinkDisplayName a
    //                yield! shrinkUser a
    //                yield! shrinkHost a
    //            } |> Seq.distinct
     
    //        Arb.fromGenShrink (generator, shrinker)
    //    with get

    ///Generates BigInteger values that are between -size and size.
    static member val BigInt = Default.Int32 |> Arb.map bigint with get

    /////Overrides the shrinker of any type to be empty, i.e. not to shrink at all.
    //static member DoNotShrink(any: Arbitrary<'T>) =
    //    any.Generator
    //    |> Gen.map DoNotShrink
    //    |> Arb.fromGen

    /// Try to derive an arbitrary instance for the given type reflectively. 
    /// Generates and shrinks values for record, union, tuple and enum types.
    /// Also generates (but doesn't shrink) values for basic classes 
    /// (i.e. either classes having a single constructor with immutable values  
    /// or DTO classes with a default constructor and public property setters).
    static member Derive<'T>(arbMap: IArbMap) :Arbitrary<'T> =
        typeof<'T>
        |> ReflectiveGenerator.reflectGenObj  arbMap.ArbFor
        |> Arb.map unbox<'T>

(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

open System
open System.Net

///Represents an int >= 0
type NonNegativeInt = NonNegativeInt of int with
    member x.Get = match x with NonNegativeInt r -> r
    static member op_Explicit(NonNegativeInt i) = i

///Represents an int > 0
type PositiveInt = PositiveInt of int with
    member x.Get = match x with PositiveInt r -> r
    static member op_Explicit(PositiveInt i) = i

///Represents an int <> 0
type NonZeroInt = NonZeroInt of int with
    member x.Get = match x with NonZeroInt r -> r
    static member op_Explicit(NonZeroInt i) = i

///Represents a float that is not NaN or Infinity.
type NormalFloat = NormalFloat of float with
    member x.Get = match x with NormalFloat f -> f
    static member op_Explicit(NormalFloat f) = f
    [<Obsolete("Will be removed in a future version.")>]
    static member get (NormalFloat f) = f

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
    static member op_Explicit(IntWithMinMax i) = i

///Represents a non-empty Set.
type NonEmptySet<'a when 'a : comparison> = NonEmptySet of Set<'a> with
    member x.Get = match x with NonEmptySet r -> r
    [<Obsolete("Will be removed in a future version.")>]
    static member toSet(NonEmptySet s) = s
    
///Represents a non-empty array.
type NonEmptyArray<'a> = NonEmptyArray of 'a[] with
    member x.Get = match x with NonEmptyArray r -> r
    [<Obsolete("Will be removed in a future version.")>]
    static member toArray(NonEmptyArray a) = a

///Represents an array whose length does not change when shrinking.
type FixedLengthArray<'a> = FixedLengthArray of 'a[] with
    member x.Get = match x with FixedLengthArray r -> r
    [<Obsolete("Will be removed in a future version.")>]
    static member toArray(FixedLengthArray a) = a

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
    [<Obsolete("Will be removed in a future version. Use From instead.")>]
    static member from f = Function<_,_>.From f
    static member From f = 
        let table = ref []
        F (table,fun x -> let y = f x in table := (x,y)::(!table); y)

///Use the generator for 'a, but don't shrink.
[<Obsolete("Renamed to DoNotShrink.")>]
type DontShrink<'a> = DontShrink of 'a

///Use the generator for 'a, but don't shrink.
type DoNotShrink<'a> = DoNotShrink of 'a

///Whereas most types are restricted by a size that grows
///as the test gets further, by applying this type the underlying
///type will ignore this size and always generate from the full range.
///Note that this only makes a difference for types that have a range -
///currently Int16, Int32, Int64 have DontSize Arbitrary instances.
///This is typically (and at least currently) only applicable for value types
///that are comparable, hence the type constraints.
type DontSize<'a when 'a : struct and 'a : comparison> = 
    DontSize of 'a with
    static member Unwrap(DontSize a) : 'a = a

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

    [<AbstractClass;Sealed>]
    type Default = class end

    let internal defaultArbitrary = 
        let empty = TypeClass<Arbitrary<obj>>.New()
        empty.Discover(onlyPublic=true,instancesType=typeof<Default>)

    let internal arbitrary = new ThreadLocal<TypeClass<Arbitrary<obj>>>(fun () -> defaultArbitrary)

    ///Register the generators that are static members of the given type.
    [<CompiledName("Register")>]
    let registerByType t = 
        let newTypeClass = arbitrary.Value.Discover(onlyPublic=true,instancesType=t)
        let result = arbitrary.Value.Compare newTypeClass
        arbitrary.Value <- arbitrary.Value.Merge newTypeClass
        result

    ///Register the generators that are static members of the type argument.
    [<CompiledName("Register")>]
    let register<'t>() = registerByType typeof<'t>

    ///Get the Arbitrary instance for the given type.
    [<CompiledName("From")>]
    let from<'Value> = arbitrary.Value.InstanceFor<'Value,Arbitrary<'Value>>()

    ///Returns a Gen<'Value>
    [<CompiledName("Generate")>]
    let generate<'Value> = from<'Value>.Generator

    ///Returns the immediate shrinks for the given value based on its type.
    [<CompiledName("Shrink")>]
    let shrink<'Value> (a:'Value) = from<'Value>.Shrinker a

    ///A generic shrinker that should work for most number-like types.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let inline shrinkNumber n =
        let (|>|) x y = abs x > abs y 
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        seq {   if n < LanguagePrimitives.GenericZero then yield -n
                if n <> LanguagePrimitives.GenericZero then yield LanguagePrimitives.GenericZero
                yield! Seq.unfold (fun st -> let st = st / two in Some (n-st, st)) n 
                        |> Seq.takeWhile ((|>|) n) }
        |> Seq.distinct

    let internal getGenerator t = arbitrary.Value.GetInstance t |> unbox<IArbitrary> |> (fun arb -> arb.GeneratorObj)

    let internal getShrink t = arbitrary.Value.GetInstance t |> unbox<IArbitrary> |> (fun arb -> arb.ShrinkerObj)

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let toGen (arb:Arbitrary<'Value>) = arb.Generator

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let toShrink (arb:Arbitrary<'Value>) = arb.Shrinker

    /// Construct an Arbitrary instance from a generator.
    /// Shrink is not supported for this type.
    [<CompiledName("From")>]
    let fromGen (gen: Gen<'Value>) : Arbitrary<'Value> =
       { new Arbitrary<'Value>() with
           override __.Generator = gen
       }

    /// Construct an Arbitrary instance from a generator and shrinker.
    [<CompiledName("From")>]
    let fromGenShrink (gen: Gen<'Value>, shrinker: 'Value -> seq<'Value>): Arbitrary<'Value> =
       { new Arbitrary<'Value>() with
           override __.Generator = gen
           override __.Shrinker a = shrinker a
       }

    // Construct an Arbitrary instance from a generator and shrinker.
    [<CompiledName("From")>]
    let fromGenShrinkFunc (gen: Gen<'Value>, shrinker: Func<'Value, seq<'Value>>): Arbitrary<'Value> =
       fromGenShrink(gen, shrinker.Invoke)
      
    ///Construct an Arbitrary instance for a type that can be mapped to and from another type (e.g. a wrapper),
    ///based on a Arbitrary instance for the source type and two mapping functions.
    [<CompiledName("Convert"); EditorBrowsable(EditorBrowsableState.Never)>]
    let convert convertTo convertFrom (a:Arbitrary<'a>) =
        { new Arbitrary<'b>() with
           override __.Generator = a.Generator |> Gen.map convertTo
           override __.Shrinker b = b |> convertFrom |> a.Shrinker |> Seq.map convertTo
       }

    /// Return an Arbitrary instance that is a filtered version of an existing arbitrary instance.
    /// The generator uses Gen.suchThat, and the shrinks are filtered using Seq.filter with the given predicate.
    [<CompiledName("Filter"); EditorBrowsable(EditorBrowsableState.Never)>]
    let filter pred (a:Arbitrary<'a>) =
        { new Arbitrary<'a>() with
           override __.Generator = a.Generator |> Gen.where pred
           override __.Shrinker b = b |> a.Shrinker |> Seq.filter pred
       }

    /// Return an Arbitrary instance that is a mapped and filtered version of an existing arbitrary instance.
    /// The generator uses Gen.map with the given mapper and then Gen.suchThat with the given predicate, 
    /// and the shrinks are filtered using Seq.filter with the given predicate.
    ///This is sometimes useful if using just a filter would reduce the chance of getting a good value
    ///from the generator - and you can map the value instead. E.g. PositiveInt.
    [<CompiledName("MapFilter"); EditorBrowsable(EditorBrowsableState.Never)>]
    let mapFilter mapper pred (a:Arbitrary<'a>) =
        { new Arbitrary<'a>() with
           override __.Generator = a.Generator |> Gen.map mapper |> Gen.where pred
           override __.Shrinker b = b |> a.Shrinker |> Seq.filter pred
       }
    
//TODO
//    /// Generate a subset of an existing set
//    let subsetOf (s: Set<'a>) : Gen<Set<'a>> =
//       gen { // Convert the set into an array
//             let setElems: 'a[] = Array.ofSeq s
//             // Generate indices into the array (up to the number of elements)
//             let! size = Gen.choose(0, s.Count)
//             let! indices = Gen.arrayOfSize (Gen.choose(0, s.Count-1)) size
//             // Extract the elements
//             let arr: 'a[] = indices |> Array.map (fun i -> setElems.[i])
//             // Construct a set (which eliminates dups)
//             return Set.ofArray arr }
//
//    /// Generate a non-empty subset of an existing (non-empty) set
//    let nonEmptySubsetOf (s: Set<'a>) : Gen<Set<'a>> =
//       gen { // Convert the set into an array
//             let setElems: 'a[] = Array.ofSeq s
//             // Generate indices into the array (up to the number of elements)
//             let! size = Gen.choose(1, s.Count)
//             let! indices = Gen.arrayOfLength (Gen.choose(0, s.Count-1)) size
//             // Extract the elements
//             let arr: 'a[] = indices |> Array.map (fun i -> setElems.[i])
//             // Construct a set (which eliminates dups)
//             return Set.ofArray arr }
  
    ///A collection of default generators.
    type Default with
        static member private fraction (a:int) (b:int) (c:int) = 
            double a + double b / (abs (double c) + 1.0) 
        
        ///Generates (), of the unit type.
        static member Unit() = 
            fromGen <| Gen.constant ()

        ///Generates an arbitrary bool.
        static member Bool() = 
            fromGen <| Gen.elements [true; false]

        ///Generates an arbitrary byte.
        static member Byte() =
            fromGenShrink ( Gen.choose (0,255) |> Gen.map byte, //this is now size independent - 255 is not enough to not cover them all anyway 
                            int >> shrink >> Seq.map byte)

        ///Generates an arbitrary signed byte.
        static member SByte() =
            fromGenShrink ( Gen.choose (-128,127) |> Gen.map sbyte,
                            int >> shrink 
                            >> Seq.filter (fun e -> -128 <= e && e <= 127) //the int shrinker shrinks -128 to 128 which overflows
                            >> Seq.map sbyte)

        ///Generate arbitrary int16 that is between -size and size.
        static member Int16() =
            Default.Int32()
            |> convert int16 int

        ///Generate arbitrary int16 that is uniformly distributed in the whole range of int16 values.
        static member DontSizeInt16() =
            let gen = Gen.choose(int Int16.MinValue, int Int16.MaxValue)
            fromGenShrink(gen, shrink)
            |> convert (int16 >> DontSize) (DontSize.Unwrap >> int)

        ///Generate arbitrary uint16 that is between 0 and size.
        static member UInt16() =
            Default.Int32()
            |> convert (abs >> uint16) int

        ///Generate arbitrary uint16 that is uniformly distributed in the whole range of uint16 values.
        static member DontSizeUInt16() =
            let gen = Gen.choose(0, int UInt16.MaxValue)
            fromGenShrink(gen, shrink)
            |> convert (uint16 >> DontSize) (DontSize.Unwrap >> int)
            
        ///Generate arbitrary int32 that is between -size and size.
        static member Int32() = 
            fromGenShrink ( Gen.sized (fun n -> Gen.choose (-n,n)),
                            shrinkNumber)

        ///Generate arbitrary int32 that is between Int32.MinValue and Int32.MaxValue
        static member DontSizeInt32() =
            //let gen = Gen.choose(Int32.MinValue, Int32.MaxValue) doesn't work with random.fs, 
            //so using this trick instead
            let gen =
                Gen.two generate<DontSize<int16>>
                |> Gen.map (fun (DontSize h,DontSize l) -> int ((uint32 h <<< 16) ||| uint32 l))
            fromGenShrink(gen, shrink)
            |> convert DontSize DontSize.Unwrap

        ///Generate arbitrary uint32 that is between 0 and size.
        static member UInt32() =
            Default.Int32()
            |> convert (abs >> uint32) int

        ///Generate arbitrary uint32 that is uniformly distributed in the whole range of uint32 values.
        static member DontSizeUInt32() =
            let gen = Gen.choose(0, int UInt32.MaxValue)
            fromGenShrink(gen, shrink)
            |> convert (uint32 >> DontSize) (DontSize.Unwrap >> int)

        ///Generate arbitrary int64 that is between -size and size.
        ///Note that since the size is an int32, this does not actually cover the full
        ///range of int64. See DontSize<int64> instead.
        static member Int64() =
            //we can be relaxed here, for the above reasons.
            from<int32>
            |> convert int64 int32

        ///Generate arbitrary int64 between Int64.MinValue and Int64.MaxValue
        static member DontSizeInt64() =
            let gen =
                Gen.two generate<DontSize<int32>>
                |> Gen.map (fun (DontSize h, DontSize l) -> (int64 h <<< 32) ||| int64 l)
            fromGenShrink (gen,shrinkNumber)
            |> convert DontSize DontSize.Unwrap
        
        ///Generate arbitrary uint64 that is between 0 and size.
        static member UInt64() =
            from<int>
            |> convert (abs >> uint64) int

        ///Generate arbitrary uint32 that is uniformly distributed in the whole range of uint32 values.
        [<Obsolete("Renamed to DoNotSizeUInt64.")>]
        static member DontSizeUInt64() =
            let gen =
                Gen.two generate<DontSize<uint32>>
                |> Gen.map (fun (DontSize h, DontSize l) -> (uint64 h <<< 32) ||| uint64 l)
            fromGenShrink (gen,shrink)
            |> convert DontSize DontSize.Unwrap
        
        ///Generate arbitrary uint32 that is uniformly distributed in the whole range of uint32 values.
        static member DoNotSizeUInt64() =
            let gen =
                Gen.two generate<DontSize<uint32>>
                |> Gen.map (fun (DontSize h, DontSize l) -> (uint64 h <<< 32) ||| uint64 l)
            fromGenShrink (gen,shrink)
            |> convert DontSize DontSize.Unwrap

        ///Generates arbitrary floats, NaN, NegativeInfinity, PositiveInfinity, Maxvalue, MinValue, Epsilon included fairly frequently.
        static member Float() = 
            let generator =
                Gen.frequency [(6, Gen.map3 Default.fraction generate generate generate)
                              ;(1, Gen.elements [ Double.NaN; Double.NegativeInfinity; Double.PositiveInfinity])
                              ;(1, Gen.elements [ Double.MaxValue; Double.MinValue; Double.Epsilon])]
            let shrinker fl =
                let (|<|) x y = abs x < abs y
                seq {   if Double.IsInfinity fl || Double.IsNaN fl then 
                            yield 0.0
                        else
                            if fl < 0.0 then yield -fl
                            let truncated = truncate fl
                            if truncated |<| fl then yield truncated }
                |> Seq.distinct
            fromGenShrink(generator, shrinker)

        ///Generates arbitrary floats, NaN, NegativeInfinity, PositiveInfinity, Maxvalue, MinValue, Epsilon included fairly frequently.
        static member Float32() = 
            let generator =
                let fraction a b c = float32 (Default.fraction a b c)
                Gen.frequency   [(6, Gen.map3 fraction generate generate generate)
                                ;(1, Gen.elements [ Single.NaN; Single.NegativeInfinity; Single.PositiveInfinity])
                                ;(1, Gen.elements [ Single.MaxValue; Single.MinValue; Single.Epsilon])]
            let shrinker fl =
                let (|<|) x y = abs x < abs y
                seq {   if Single.IsInfinity fl || Single.IsNaN fl then 
                            yield 0.0f
                        else
                            if fl < 0.0f then yield -fl
                            let truncated = truncate fl
                            if truncated |<| fl then yield truncated }
                |> Seq.distinct
            fromGenShrink (generator,shrinker)

        ///Generate arbitrary decimal.
        static member Decimal() =
            let genDecimal = 
                gen {
                    let! lo = generate
                    let! mid = generate
                    let! hi = generate
                    let! isNegative = generate
                    let! scale = Gen.choose(0, 28) |> Gen.map byte
                    return System.Decimal(lo, mid, hi, isNegative, scale)
                }
            let shrinkDecimal d =
                let (|<|) x y = abs x < abs y
                seq {
                    if d < 0m then yield -d
                    let truncated = truncate d
                    if truncated |<| d then yield truncated
                }
            fromGenShrink (genDecimal, shrinkDecimal)
            
        ///Generates arbitrary chars, between ASCII codes Char.MinValue and 127.
        static member Char() = 
            let generator = Gen.choose (int Char.MinValue, 127) |> Gen.map char
            let shrinker c = seq { for c' in ['a';'b';'c'] do if c' < c || not (Char.IsLower c) then yield c' }
            fromGenShrink (generator, shrinker)

        ///Generates arbitrary strings, which are lists of chars generated by Char.
        static member String() = 
            let generator = Gen.frequency [(9, Gen.map (fun (chars:char[]) -> new String(chars)) generate);(1, Gen.constant null)]
            let shrinker (s:string) = 
                    match s with
                    | null -> Seq.empty
                    | _ -> s.ToCharArray() |> Array.toList |> shrink |> Seq.map (fun chars -> new String(List.toArray chars))
            fromGenShrink (generator,shrinker)

        ///Generate an option value that is 'None' 1/8 of the time.
        static member Option() = 
            { new Arbitrary<option<'a>>() with
                override __.Generator = Gen.frequency [(1, gen { return None }); (7, Gen.map Some generate)]
                override __.Shrinker o =
                    match o with
                    | Some x -> seq { yield None; for x' in shrink x -> Some x' }
                    | None  -> Seq.empty
            }

        ///Generate underlying values that are not null.
        static member NonNull() =
            let inline notNull x = not (LanguagePrimitives.PhysicalEquality null x)
            { new Arbitrary<NonNull<'a>>() with
                override __.Generator = 
                    generate |> Gen.where notNull |> Gen.map NonNull
                override __.Shrinker (NonNull o) = 
                    shrink o |> Seq.where notNull |> Seq.map NonNull
            }

        ///Generate a nullable value that is null 1/8 of the time.
        static member Nullable() = 
            { new Arbitrary<Nullable<'a>>() with
                override __.Generator = Gen.frequency [(1, gen { return Nullable() }); (7, Gen.map Nullable generate)]
                override __.Shrinker o =
                    if o.HasValue
                        then seq { yield Nullable(); for x' in shrink o.Value -> Nullable x' }
                        else Seq.empty
            }

        ///Generate a list of values. The size of the list is between 0 and the test size + 1.
        static member FsList() = 
            { new Arbitrary<list<'a>>() with
                override __.Generator = Gen.listOf generate
                override __.Shrinker l =
                    match l with
                    | [] ->         Seq.empty
                    | (x::xs) ->    seq { yield xs
                                          for xs' in shrink xs -> x::xs'
                                          for x' in shrink x -> x'::xs }
            }

        ///Generate an object - a boxed char, string or boolean value.
        static member Object() =
            { new Arbitrary<obj>() with
                override __.Generator = 
                    Gen.oneof [ Gen.map box <| generate<char> ; Gen.map box <| generate<string>; Gen.map box <| generate<bool> ]
                override __.Shrinker o =
                    if o = null then Seq.empty
                    else
                        seq {
                            match o with
                            | :? char as c -> yield box true; yield box false; yield! shrink c |> Seq.map box
                            | :? string as s -> yield box true; yield box false; yield! shrink s |> Seq.map box
                            | :? bool -> yield! Seq.empty
                            | _ -> failwith "Unknown type in shrink of obj"
                        }
            }
        ///Generate a rank 1 array.
        static member Array() =
            { new Arbitrary<'a[]>() with
                override __.Generator = Gen.arrayOf generate
                override __.Shrinker a = a |> Array.toList |> shrink |> Seq.map List.toArray
            }

        ///Generate a rank 2, zero based array.
        static member Array2D() = 
            let shrinkArray2D (arr:_[,]) =
                let removeRow r (arr:_[,]) =
                    Array2D.init (Array2D.length1 arr-1) (Array2D.length2 arr) (fun i j -> if i < r then arr.[i,j] else arr.[i+1,j])
                let removeCol c (arr:_[,]) =
                    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr-1) (fun i j -> if j < c then arr.[i,j] else arr.[i,j+1])
                seq { for r in 1..Array2D.length1 arr do yield removeRow r arr
                      for c in 1..Array2D.length2 arr do yield removeCol c arr
                      for i in 0..Array2D.length1 arr-1 do //hopefully the matrix is shrunk considerably before we get here...
                        for j in 0..Array2D.length2 arr-1 do
                            for elem in shrink arr.[i,j] do
                                let shrunk = Array2D.copy arr
                                shrunk.[i,j] <- elem
                                yield shrunk
                    }
                            
            fromGenShrink (Gen.array2DOf generate,shrinkArray2D)

         ///Generate a function value. Function values can be generated for types 'a->'b where 'b has an Arbitrary instance.
         ///There is no shrinking for function values.
        static member Arrow() = 
            let gen = let vfun = Gen.variant in Gen.promote (fun a -> vfun a generate)
            { new Arbitrary<'a->'b>() with
                override __.Generator = gen
            }

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
            let gen = let vfun = Gen.variant
                      Gen.promote (fun a -> vfun a (Gen.frequency [(exc.Length, generate);(exc.Length, throwExc)]))
                      |> Gen.map ThrowingFunction
            { new Arbitrary<ThrowingFunction<'a,'b>>() with
                override __.Generator = gen
            }

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
#if PCL
#else
                                        NotFiniteNumberException()
                                        IO.DirectoryNotFoundException()
                                        IO.FileLoadException()
                                        StackOverflowException()
                                        KeyNotFoundException()
                                        IO.PathTooLongException()
#endif
                                     |]

        ///Generate a Function value that can be printed and shrunk. Function values can be generated for types 'a->'b 
        ///where 'b has an Arbitrary instance.
        static member Function() =
            { new Arbitrary<Function<'a,'b>>() with
                override __.Generator = Gen.map Function<'a,'b>.From generate
                override __.Shrinker f = 
                    let update x' y' f x = if x = x' then y' else f x
                    seq { for (x,y) in f.Table do 
                            for y' in shrink y do 
                                yield Function<'a,'b>.From (update x y' f.Value) }
            }

        ///Generates a Func'1.
        static member SystemFunc() =
            Default.Fun()
            |> convert (fun f -> Func<_>(f)) (fun f -> f.Invoke)

        ///Generates a Func'2.
        static member SystemFunc1() =
            Default.Fun()
            |> convert (fun f -> Func<_,_>(f)) (fun f -> f.Invoke)

        ///Generates a Func'3.
        static member SystemFunc2() =
            Default.Fun()
            |> convert (fun f -> Func<_,_,_>(f)) (fun f a b -> f.Invoke(a,b))

        ///Generates a Func'4.
        static member SystemFunc3() =
            Default.Fun()
            |> convert (fun f -> Func<_,_,_,_>(f)) (fun f a b c -> f.Invoke(a,b,c))

        ///Generates an Action'0
        static member SystemAction() =
            Default.Fun()
            |> convert (fun f -> Action(f)) (fun f -> f.Invoke)

        ///Generates an Action'1
        static member SystemAction1() =
            Default.Fun()
            |> convert (fun f -> Action<_>(f)) (fun f -> f.Invoke)

        ///Generates an Action'2
        static member SystemAction2() =
            Default.Fun()
            |> convert (fun f -> Action<_,_>(f)) (fun f a b -> f.Invoke(a,b))

        ///Generates an Action'3
        static member SystemAction3() =
            Default.Fun()
            |> convert (fun f -> Action<_,_,_>(f)) (fun f a b c -> f.Invoke(a,b,c))

        ///Generates an arbitrary DateTime between 1900 and 2100. 
        ///A DateTime is shrunk by removing its second, minute and hour components.
        static member DateTime() = 
            let genDate = gen {  let! y = Gen.choose(1900,2100)
                                 let! m = Gen.choose(1, 12)
                                 let! d = Gen.choose(1, DateTime.DaysInMonth(y, m))
                                 let! h = Gen.choose(0,23)
                                 let! min = Gen.choose(0,59)
                                 let! sec = Gen.choose(0,59)
                                 return DateTime(y, m, d, h, min, sec) }
            let shrinkDate (d:DateTime) = 
                if d.Second <> 0 then
                    seq { yield DateTime(d.Year,d.Month,d.Day,d.Hour,d.Minute,0) }
                elif d.Minute <> 0 then
                    seq { yield DateTime(d.Year,d.Month,d.Day,d.Hour,0,0) }
                elif d.Hour <> 0 then
                    seq { yield DateTime(d.Year,d.Month,d.Day) }
                else
                    Seq.empty
            fromGenShrink (genDate,shrinkDate)

        ///Generates an arbitrary TimeSpan. A TimeSpan is shrunk by removing days, hours, minutes, second and milliseconds.
        static member TimeSpan() =
            let genTimeSpan = generate |> Gen.map (fun (DontSize ticks) -> TimeSpan ticks)
            let shrink (t: TimeSpan) = 
                if t.Days > 0 then
                    seq { yield TimeSpan(0, t.Hours, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Hours > 0 then
                    seq { yield TimeSpan(0, 0, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Minutes > 0 then
                    seq { yield TimeSpan(0, 0, 0, t.Seconds, t.Milliseconds) }
                elif t.Seconds > 0 then
                    seq { yield TimeSpan(0, 0, 0, 0, t.Milliseconds) }
                elif t.Milliseconds > 0 then
                    seq { yield TimeSpan(0L) }
                else
                    Seq.empty
            fromGenShrink (genTimeSpan, shrink)

        ///Generates an arbitrary DateTimeOffset between 1900 and 2100. 
        /// A DateTimeOffset is shrunk first by shrinking its offset, then by removing its second, minute and hour components.
        static member DateTimeOffset() =
            let genTimeZone = gen {
                                let! hours = Gen.choose(-14, 14)
                                let! minutes = 
                                    if abs hours = 14 then 
                                        Gen.constant 0
                                    else 
                                        Gen.choose(0, 59)
                                return TimeSpan(hours, minutes, 0) }
            let shrinkTimeZone (t: TimeSpan) =
                shrink t |> Seq.where (fun z -> z.Hours > 0 || z.Minutes > 0)
            let genDate = gen { 
                            let! t = generate<DateTime>
                            let! tz = genTimeZone
                            return DateTimeOffset(t, tz) }
            let shrink (d: DateTimeOffset) =
                seq {
                    for ts in shrinkTimeZone d.Offset ->
                        DateTimeOffset(d.DateTime, ts)
                    if d.Offset <> TimeSpan.Zero then
                        yield DateTimeOffset(d.DateTime, TimeSpan.Zero)
                    for dt in shrink d.DateTime ->
                        DateTimeOffset(dt, TimeSpan.Zero) }
            fromGenShrink (genDate, shrink)

        static member KeyValuePair() =
            let genKeyValuePair =
                gen {
                    let! key = generate
                    let! value = generate
                    return KeyValuePair(key, value)
                }
            let shrinkKeyValuePair (kvp:KeyValuePair<_,_>) = 
                seq { for key in shrink kvp.Key do
                        for value in shrink kvp.Value do
                            yield new KeyValuePair<_,_>(key, value) }
            fromGenShrink(genKeyValuePair,shrinkKeyValuePair)

        static member NonNegativeInt() =
           Default.Int32()
           |> mapFilter abs (fun i -> i >= 0)
           |> convert NonNegativeInt int

        static member PositiveInt() =
            Default.Int32()
            |> mapFilter abs (fun i -> i > 0)
            |> convert PositiveInt int

        static member NonZeroInt() =
           Default.Int32()
            |> filter ((<>) 0)
            |> convert NonZeroInt int

        /// Generates an Float (without NaN, Infinity)
        static member NormalFloat() =
            Default.Float()
            |> filter (fun f -> not <| System.Double.IsNaN(f) &&
                                not <| System.Double.IsInfinity(f))
            |> convert NormalFloat float

        static member IntWithMinMax() =
            { new Arbitrary<IntWithMinMax>() with
                override __.Generator = Gen.frequency [(1 ,Gen.elements [Int32.MaxValue; Int32.MinValue])
                                                       (10,generate) ] 
                                       |> Gen.map IntWithMinMax
                override __.Shrinker (IntWithMinMax i) = shrink i |> Seq.map IntWithMinMax }

        ///Generates an interval between two non-negative integers.
        static member Interval() =
            gen { let! start,offset = Gen.two generate
                  return Interval (abs start,abs start+abs offset) }//TODO: shrinker
            |> fromGen


        static member StringWithoutNullChars() =
            Default.String()
            |> filter (not << String.exists ((=) '\000'))
            |> convert StringNoNulls string

        static member NonEmptyString() =
            Default.String()
            |> filter (fun s -> not (String.IsNullOrEmpty s) && not (String.exists ((=) '\000') s))
            |> convert NonEmptyString string

        static member Set() = 
            Default.FsList()
            |> convert Set.ofList Set.toList

        static member Map() = 
            Default.FsList()
            |> convert Map.ofList Map.toList

        static member NonEmptyArray() =
            Default.Array()
            |> filter (fun a -> Array.length a > 0)
            |> convert NonEmptyArray (fun (NonEmptyArray s) -> s)

        static member NonEmptySet() =
            Default.Set()
            |> filter (not << Set.isEmpty) 
            |> convert NonEmptySet (fun (NonEmptySet s) -> s)

        ///Arrays whose length does not change when shrinking.
        static member FixedLengthArray() =
            { new Arbitrary<'a[]>() with
                override __.Generator = generate
                override __.Shrinker a = a |> Seq.mapi (fun i x -> shrink x |> Seq.map (fun x' ->
                                                           let data' = Array.copy a
                                                           data'.[i] <- x'
                                                           data')
                                                   ) |> Seq.concat
            }
            |> convert FixedLengthArray (fun (FixedLengthArray a) -> a)

        /// Generate a System.Collections.Generic.List of values.
        static member List() =
            Default.FsList() 
            |> convert Enumerable.ToList Seq.toList

        /// Generate a System.Collections.Generic.IList of values.
        static member IList() =
            Default.List()
            |> convert (fun x -> x :> _ IList) (fun x -> x :?> _ List)

        /// Generate a System.Collections.Generic.ICollection of values.
        static member ICollection() =
            Default.List()
            |> convert (fun x -> x :> _ ICollection) (fun x -> x :?> _ List)

        /// Generate a System.Collections.Generic.Dictionary of values.
        /// Shrinks by reducing the number of elements
        static member Dictionary() =
            let genDictionary = 
                gen {
                    let! keys = Gen.listOf generate |> Gen.map (Seq.where (fun x -> not (obj.Equals(x, null))) >> Seq.distinct >> Seq.toList)
                    let! values = Gen.arrayOfLength keys.Length generate
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
            fromGenShrink (genDictionary, shrinkDictionary)

        /// Generate a System.Collections.Generic.IDictionary of values.
        /// Shrinks by reducing the number of elements
        static member IDictionary() =
            Default.Dictionary()
            |> convert (fun x -> x :> IDictionary<_,_>) (fun x -> x :?> Dictionary<_,_>)

        static member Culture() =
#if PCL
            let cultureNames = [
                "af"; "af-ZA";
                "am"; "am-ET";
                "ar"; "ar-AE"; "ar-BH"; "ar-DZ"; "ar-EG"; "ar-IQ"; "ar-JO"; "ar-KW"; "ar-LB"; "ar-LY"; "ar-MA"; "ar-OM"; "ar-QA"; "ar-SA"; "ar-SY"; "ar-TN"; "ar-YE"; "arn"; "arn-CL";
                "as"; "as-IN";
                "az"; "az-Cyrl"; "az-Cyrl-AZ"; "az-Latn"; "az-Latn-AZ";
                "ba"; "ba-RU";
                "be"; "be-BY";
                "bg"; "bg-BG";
                "bn"; "bn-BD"; "bn-IN";
                "bo"; "bo-CN";
                "br"; "br-FR";
                "bs"; "bs-Cyrl"; "bs-Cyrl-BA"; "bs-Latn"; "bs-Latn-BA";
                "ca"; "ca-ES";
                "co"; "co-FR";
                "cs"; "cs-CZ";
                "cy"; "cy-GB";
                "da"; "da-DK";
                "de"; "de-AT"; "de-CH"; "de-DE"; "de-LI"; "de-LU";
                "dsb"; "dsb-DE";
                "dv"; "dv-MV";
                "el"; "el-GR";
                "en"; "en-029"; "en-AU"; "en-BZ"; "en-CA"; "en-GB"; "en-IE"; "en-IN"; "en-JM"; "en-MY"; "en-NZ"; "en-PH"; "en-SG"; "en-TT"; "en-US"; "en-ZA"; "en-ZW";
                "es"; "es-AR"; "es-BO"; "es-CL"; "es-CO"; "es-CR"; "es-DO"; "es-EC"; "es-ES"; "es-GT"; "es-HN"; "es-MX"; "es-NI"; "es-PA"; "es-PE"; "es-PR"; "es-PY"; "es-SV"; "es-US"; "es-UY"; "es-VE";
                "et"; "et-EE";
                "eu"; "eu-ES";
                "fa"; "fa-IR";
                "fi"; "fi-FI"; "fil"; "fil-PH";
                "fo"; "fo-FO";
                "fr"; "fr-BE"; "fr-CA"; "fr-CH"; "fr-FR"; "fr-LU"; "fr-MC";
                "fy"; "fy-NL";
                "ga"; "ga-IE";
                "gd"; "gd-GB";
                "gl"; "gl-ES";
                "gsw"; "gsw-FR";
                "gu"; "gu-IN";
                "ha"; "ha-Latn"; "ha-Latn-NG";
                "he"; "he-IL";
                "hi"; "hi-IN";
                "hr"; "hr-BA"; "hr-HR";
                "hsb"; "hsb-DE";
                "hu"; "hu-HU";
                "hy"; "hy-AM";
                "id"; "id-ID";
                "ig"; "ig-NG";
                "ii"; "ii-CN";
                "is"; "is-IS";
                "it"; "it-CH"; "it-IT";
                "iu"; "iu-Cans"; "iu-Cans-CA"; "iu-Latn"; "iu-Latn-CA";
                "ja"; "ja-JP";
                "ka"; "ka-GE";
                "kk"; "kk-KZ";
                "kl"; "kl-GL";
                "km"; "km-KH";
                "kn"; "kn-IN";
                "ko"; "ko-KR"; "kok"; "kok-IN";
                "ky"; "ky-KG";
                "lb"; "lb-LU";
                "lo"; "lo-LA";
                "lt"; "lt-LT";
                "lv"; "lv-LV";
                "mi"; "mi-NZ";
                "mk"; "mk-MK";
                "ml"; "ml-IN";
                "mn"; "mn-Cyrl"; "mn-MN"; "mn-Mong"; "mn-Mong-CN";
                "moh"; "moh-CA";
                "mr"; "mr-IN";
                "ms"; "ms-BN"; "ms-MY";
                "mt"; "mt-MT";
                "nb"; "nb-NO";
                "ne"; "ne-NP";
                "nl"; "nl-BE"; "nl-NL";
                "nn"; "nn-NO";
                "no";
                "nso"; "nso-ZA";
                "oc"; "oc-FR";
                "or"; "or-IN";
                "pa"; "pa-IN";
                "pl"; "pl-PL";
                "prs"; "prs-AF";
                "ps"; "ps-AF";
                "pt"; "pt-BR"; "pt-PT";
                "qut"; "qut-GT"; "quz"; "quz-BO"; "quz-EC"; "quz-PE";
                "rm"; "rm-CH";
                "ro"; "ro-RO";
                "ru"; "ru-RU";
                "rw"; "rw-RW";
                "sa"; "sa-IN"; "sah"; "sah-RU";
                "se"; "se-FI"; "se-NO"; "se-SE";
                "si"; "si-LK";
                "sk"; "sk-SK";
                "sl"; "sl-SI";
                "sma"; "sma-NO"; "sma-SE"; "smj"; "smj-NO"; "smj-SE"; "smn"; "smn-FI"; "sms"; "sms-FI";
                "sq"; "sq-AL";
                "sr"; "sr-Cyrl"; "sr-Cyrl-BA"; "sr-Cyrl-CS"; "sr-Cyrl-ME"; "sr-Cyrl-RS"; "sr-Latn"; "sr-Latn-BA"; "sr-Latn-CS"; "sr-Latn-ME"; "sr-Latn-RS";
                "sv"; "sv-FI"; "sv-SE";
                "sw"; "sw-KE";
                "syr"; "syr-SY";
                "ta"; "ta-IN";
                "te"; "te-IN";
                "tg"; "tg-Cyrl"; "tg-Cyrl-TJ";
                "th"; "th-TH";
                "tk"; "tk-TM";
                "tn"; "tn-ZA";
                "tr"; "tr-TR";
                "tt"; "tt-RU";
                "tzm"; "tzm-Latn"; "tzm-Latn-DZ";
                "ug"; "ug-CN";
                "uk"; "uk-UA";
                "ur"; "ur-PK";
                "uz"; "uz-Cyrl"; "uz-Cyrl-UZ"; "uz-Latn"; "uz-Latn-UZ";
                "vi"; "vi-VN";
                "wo"; "wo-SN";
                "xh"; "xh-ZA";
                "yo"; "yo-NG";
                "zh"; "zh-CN"; "zh-HK"; "zh-Hans"; "zh-Hant"; "zh-MO"; "zh-SG"; "zh-TW";
                "zu"; "zu-ZA";]
            let cultures = 
                cultureNames |> Seq.choose (fun name -> try Some (CultureInfo name) with _ -> None)
                      |> Seq.append [ CultureInfo.InvariantCulture; 
                                      CultureInfo.CurrentCulture; 
                                      CultureInfo.CurrentUICulture; 
                                      CultureInfo.DefaultThreadCurrentCulture;
                                      CultureInfo.DefaultThreadCurrentUICulture; ]
            let genCulture = Gen.elements cultures
#else
            let genCulture = Gen.elements (CultureInfo.GetCultures (CultureTypes.NeutralCultures ||| CultureTypes.SpecificCultures))
#endif
            let shrinkCulture =
                Seq.unfold <| fun c -> if c = null || c = CultureInfo.InvariantCulture || c.Parent = null
                                            then None
                                            else Some (c.Parent, c.Parent)
            fromGenShrink (genCulture, shrinkCulture)

        static member Guid() =
            gen {
                let! a = generate
                let! b = generate
                let! c = generate
                let! d = generate
                let! e = generate
                let! f = generate
                let! g = generate
                let! h = generate
                let! i = generate
                let! j = generate
                let! k = generate
                return Guid((a: int),b,c,d,e,f,g,h,i,j,k)
            } |> fromGen

#if PCL
#else
        static member IPAddress() =
            let generator = generate |> Gen.arrayOfLength 4 |> Gen.map IPAddress
            let shrinker (a:IPAddress) = a.GetAddressBytes() |> shrink |> Seq.filter (fun x -> Seq.length x = 4) |> Seq.map IPAddress
        
            fromGenShrink (generator, shrinker)
#endif

        ///Arbitray instance for BigInteger.
        static member BigInt() =
            Default.Int32()
            |> convert bigint int

        ///Overrides the shrinker of any type to be empty, i.e. not to shrink at all.
        [<Obsolete("Renamed to DoNotShrink.")>]
        static member DontShrink() =
            generate |> Gen.map DoNotShrink |> fromGen

        ///Overrides the shrinker of any type to be empty, i.e. not to shrink at all.
        static member DoNotShrink() =
            generate |> Gen.map DoNotShrink |> fromGen
            
        ///Try to derive an arbitrary instance for the given type reflectively. 
        ///Generates and shrinks values for record, union, tuple and enum types.
        ///Also generates (but doesn't shrink) values for immutable classes 
        ///(i.e. single constructor, no mutable properties or fields).
        static member Derive() =
            //taking out this generator makes sure that the memoization table in reflectGenObj
            //is used properly.
            let generator = ReflectArbitrary.reflectGenObj getGenerator
            { new Arbitrary<'a>() with
                override __.Generator = generator typeof<'a> |> Gen.map unbox<'a>
                override __.Shrinker a = ReflectArbitrary.reflectShrink getShrink a
            }
            

    
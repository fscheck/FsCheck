namespace FsCheck

open System.Net

// This file contains a number of convenience types that have built-in generators.


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

// Represents a string that is not null, empty or consists only of white-space characters and does not contain any null characters ('\000')
type NonWhiteSpaceString = NonWhiteSpaceString of string with
    member x.Get = match x with NonWhiteSpaceString s -> s
    override x.ToString() = x.Get

///Represents a string that can be serializable as a XML value.
type XmlEncodedString = XmlEncodedString of string with
    member x.Get = match x with XmlEncodedString v -> v
    override x.ToString() = x.Get

///Represents a unicode char.
type UnicodeChar = UnicodeChar of char with
    member x.Get = match x with UnicodeChar c -> c
    override x.ToString() = string x.Get

///Represents a string that can contain unicode characters.
type UnicodeString = UnicodeString of string with
    member x.Get = match x with UnicodeString v -> v
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
///currently integer types, TimeSpan and Decimal have DoNotSize Arbitrary instances.
///This is typically (and at least currently) only applicable for value types
///that are comparable, hence the type constraints.
type DoNotSize<'a when 'a : struct and 'a : comparison> = 
    DoNotSize of 'a with
    static member Unwrap(DoNotSize a) : 'a = a

type IPv4Address = IPv4Address of IPAddress
type IPv6Address = IPv6Address of IPAddress

type HostName = HostName of string with
    override x.ToString () = match x with HostName s -> s

namespace FsCheck.FSharp

open FsCheck

[<AutoOpen>]
module ArbPatterns =
    let (|Fun|) (f:Function<'a,'b>) = f.Value


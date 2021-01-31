
namespace FsCheck

// base types: Gen<T>, Shrink<T> and Arbitrary<'T>

type Arb<'T> = A of 'T

namespace FsCheck.FSharp
// modules for F# usage of base types: Gen, Shrink. Arb
// for pipleine style

open FsCheck

module Arb =

    let one = A 1

    let map f (A t) = A (f t)



namespace FsCheck.Fluent
// classes for .NET usage of base types: Gen, Shrink, Arb  - defined as extension members
// for fluent style

open System.Runtime.CompilerServices

[<Extension>]
type Arb() =
    static member One = FsCheck.FSharp.Arb.one

    [<Extension>]
    static member Map(a, f) = FsCheck.FSharp.Arb.map f a

namespace FsCheck

open System.Runtime.InteropServices

type NonZeroInt = NonZeroInt of int with
    static member Arbitrary() =
        NonZeroInt 10 |> A
    member t.Get = let (NonZeroInt i) = t in i
    member t.Deconstruct([<Out>] i:byref<int>) = i <- t.Get

open System

type ArbContext = AC of System.Collections.Generic.IDictionary<Type,Arb<obj>>

module ArbContext =
    let empty = AC null

//    let add a 

//    let remove 
    
//    let  = derive
    

namespace MyFSharpProject

open FsCheck.FSharp

module M =
    let a = Arb.one |> Arb.map id


namespace MyCSharpProject

open FsCheck.Fluent

module M =
    let a = Arb.One.Map(id)


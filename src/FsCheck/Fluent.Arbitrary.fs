namespace FsCheck.Fluent

open System
open System.Runtime.CompilerServices

open FsCheck
open FsCheck.FSharp


[<Extension;Sealed;AbstractClass>]
type Arb private() =

    
    [<Extension>]
    static member Select (arb:Arbitrary<'T>, select: Func<_,_>) :Arbitrary<'U> =
        Arb.map select.Invoke arb

    /// Return an Arbitrary instance that is a filtered version of an existing arbitrary instance.
    /// The generator uses Gen.suchThat, and the shrinks are filtered using Seq.filter with the given predicate.
    [<Extension>]
    static member Where (arb:Arbitrary<'T>, filter: Func<_,_>) =
        Arb.filter filter.Invoke arb

namespace FsCheck.Experimental.Test

module ArbitraryConstructor =

    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open System
    open FsCheck.Experimental
    open FsCheck.Experimental.ArbitraryConstructor
    
    type SystemOriginSpec =
        static member gen() =
            let assembly = typeof<string>.Assembly
            let spec = TypeSpec(assembly |> Seq.singleton)
            spec |> typeGen |> Arb.fromGen   

    [<Property(Arbitrary = [|typeof<SystemOriginSpec>|])>]
    let ``Can retrieve types`` (t : Type) =
        true |> Prop.collect t
    
    [<Property(MaxTest = 1)>]
    let ``Object parent equals to usnspecified parent``() =
        let assembly = typeof<string>.Assembly
        let specA = TypeSpec(assembly |> Seq.singleton, requiredParents = [typeof<obj>])
        let specB = TypeSpec(assembly |> Seq.singleton, requiredParents = [])
        ( specA |> typesMatchingSpec |> Seq.length) = (specB |> typesMatchingSpec |> Seq.length)

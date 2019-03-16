namespace FsCheck.Experimental.Test

module ArbitraryConstructor =

    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open System
    open FsCheck.Experimental
    open FsCheck.Experimental.ArbitraryConstructor

    [<Fact>]
    let ``Can retrieve types``() =
        let assembly = typeof<string>.Assembly
        let constr = TypeConstraints(assembly |> Seq.singleton)
        typeFrom constr |> ignore
    
    [<Property(MaxTest = 1)>]
    let ``Object parent equals to usnspecified parent``() =
        let assembly = typeof<string>.Assembly
        let constrA = TypeConstraints(assembly |> Seq.singleton, requiredParents = [typeof<obj>])
        let constrB = TypeConstraints(assembly |> Seq.singleton, requiredParents = [])
        (typeFrom constrA |> Seq.length) = (typeFrom constrB |> Seq.length)

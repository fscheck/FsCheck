namespace FsCheck.Experimental

open System.Reflection
open System
open FsCheck

//Is not record to easily add new params
///Represents constraints for generated type
type TypeSpec(origin : Assembly seq, 
                     ?requiredAttributes : Type seq,
                     ?requiredParents : Type seq ) =
    member __.Origin = origin
    member __.RequiredAttributes = defaultArg requiredAttributes Seq.empty
    member __.RequiredParents = defaultArg requiredParents Seq.empty


module ArbitraryConstructor =

#if NETSTANDARD2_0 || NET452 //Attribute.IsDefined is not available in lower versions, fix later 
    let typesMatchingSpec (spec : TypeSpec) =
        let origin = spec.Origin
        let allTypes = 
            origin
            |> Seq.map (fun assembly -> assembly.GetExportedTypes() |> Seq.ofArray)
            |> Seq.concat
        let withAttributes =
            let hasAttribute (t : Type) (a : Type) =
                Attribute.IsDefined(t, a)
            allTypes |> Seq.filter (fun t -> Seq.forall (hasAttribute t) spec.RequiredAttributes)
        let withParents =
            let isDerived (t : Type) (b : Type) =
                b.IsAssignableFrom t
            withAttributes |> Seq.filter (fun t -> Seq.forall (isDerived t) spec.RequiredParents)
        withParents
    
    let typeGen spec = typesMatchingSpec spec |> Gen.elements
#endif        
     
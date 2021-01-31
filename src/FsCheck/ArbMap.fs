namespace FsCheck

open System

open FsCheck.FSharp
open FsCheck.Internals

/// Maps types to Arbitrary instances for that type.
/// Once constructed, the map is immutable.
type IArbMap =
    abstract ArbFor: Type -> Arbitrary<obj>
    abstract ArbFor<'T> : unit -> Arbitrary<'T>

// Note: the only implementation of IArbMap. The main reason this type is
// not exposed directly, is because that gives annoying type name clashes
// between the ArbMap type, the ArbMap module (which can be addressed with ModuleSuffix),
// and the ArbMap extension type in FsCheck.Fluent.
type internal ArbMap internal (typ: Type, ?existingMap: ArbMap) as this =
    let finder =
        match existingMap with
        | None ->
            TypeClass.TypeClass<Arbitrary<obj>>
                .New(injectParameters = true)
        | Some arbFinder -> arbFinder.ArbFinder

    let result =
        finder.DiscoverAndMerge(onlyPublic = false, instancesType = typ, newInjectedConfigs = [| this |])

    member private _.ArbFinder = result

    // for testing purposes
    member internal _.MemoizedInstances = result.MemoizedInstances

    interface IArbMap with
        member _.ArbFor t =
            result.GetInstance t
            |> unbox<IArbitrary>
            |> (fun arb -> Arb.fromGenShrink (arb.GeneratorObj, arb.ShrinkerObj))

        member _.ArbFor<'TArb>() =
            result.InstanceFor<'TArb, Arbitrary<'TArb>>()

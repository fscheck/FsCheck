namespace FsCheck

open System

open FsCheck.FSharp
open FsCheck.Internals

/// Maps types to Arbitrary instances for that type.
/// Once constructed, the map is immutable.
type IArbMap =
    abstract ArbFor: Type -> Arbitrary<obj>
    abstract ArbFor<'T> : unit -> Arbitrary<'T>
    abstract MergeFactory : Func<'a,Arbitrary<'b>> -> IArbMap


// Note: the only implementation of IArbMap. The main reason this type is
// not exposed directly, is because that gives annoying type name clashes
// between the ArbMap type, the ArbMap module (which can be addressed with ModuleSuffix),
// and the ArbMap extension type in FsCheck.Fluent.
type internal ArbMap private (init: ArbMapInit) as this=
    
    let finder = 
        match init with
        | FromTypeClass tc -> tc.Merge(TypeClass.TypeClass(injectedConfigs = [| this |]))
        | FromDiscover (typ, existingMap) -> 
            let finder =
                match existingMap with
                | None ->
                    TypeClass.TypeClass<Arbitrary<obj>>
                        .New(injectParameters = true)
                | Some arbFinder -> arbFinder.ArbFinder
            
            finder.DiscoverAndMerge(onlyPublic = false, instancesType = typ, newInjectedConfigs = [| this |])

    member private _.ArbFinder = finder

    internal new (typ:Type, ?existingMap:ArbMap) = 
        ArbMap(FromDiscover (typ, existingMap))

    // for testing purposes
    member internal _.MemoizedInstances = finder.MemoizedInstances

    interface IArbMap with
        member _.ArbFor t =
            finder.GetInstance t
            |> unbox<IArbitrary>
            |> (fun arb -> Arb.fromGenShrink (arb.GeneratorObj, arb.ShrinkerObj))

        member _.ArbFor<'TArb>() =
            finder.InstanceFor<'TArb, Arbitrary<'TArb>>()

        member _.MergeFactory(factory: Func<'a,Arbitrary<'b>>) =
            ArbMap(FromTypeClass (finder.MergeFactory(factory))) :> IArbMap
and 
    // There is a tricky mutual dependence between the ArbMap instance and
    // the typeclass it relies on which makes construction difficult
    // - Static factories are out. There must be a reference to the ArbMap being initialized in the
    //   typeclass needed for initializing the ArbMap, else the typeclass wont resolve expected factory scenarios.
    // - Two independent constructors fail because referencing this in the constructor causes recursive initialization 
    //   - Splitting discover and registration of `this` as a config parameter causes essential 
    //     factories to be filtered out during discovery
    // - Using a mutable finder field could work, but would allow mutation of a type that is meant to be immutable
    private ArbMapInit = 
    | FromTypeClass of TypeClass.TypeClass<Arbitrary<obj>>
    | FromDiscover of (Type * ArbMap option)
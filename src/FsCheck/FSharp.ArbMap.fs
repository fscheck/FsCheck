namespace FsCheck.FSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix);RequireQualifiedAccess>]
module ArbMap =

    open System
    open FsCheck

    /// The immutable built-in mapping type to Arbitrary for that type.
    let defaults = ArbMap(typeof<Internals.Default>) :> IArbMap // need value here, otherwise it gets rebuilt on every call to ArbMap.Default.

    let mergeWithType (instancesType: Type) (existingMap: IArbMap) =
        ArbMap(instancesType, existingMap :?> ArbMap) :> IArbMap

    /// Return a new Type to Arbitrary map that merges the existing map with new Arbitrary<'T> instances
    /// discovered on the given type argument 'TArb.
    /// The new Arbitrary instances take precedence over the ones for the same type
    /// in the existing map.
    ///
    /// instancesType should have static methods or properties that return Arbitrary<'T>
    /// instances. The methods or properties can have any name, and there can be any number of them.
    ///
    /// A method may return Arbitrary instances for generic types, e.g. Arbitrary<KeyValuePair<'K,'V>>.
    /// In that case, the implementation needs to obtain an Arbitrary<'Key> and Arbitrary<'Value> - it can declare
    /// parameters of type Arbitrary<'T> to obtain them. Arbitrary instance of the correct type are automatically
    /// injected on construction of a particular type.
    let mergeWith<'TArb> (existingMap: IArbMap) = mergeWithType typeof<'TArb> existingMap
 

    let arbitrary<'T> (arbMap:IArbMap) = arbMap.ArbFor<'T>()
    let generate<'T> arbMap = (arbitrary<'T> arbMap).Generator

    
//namespace FsCheck

//module ArbMapExtensions =

//    type ArbMap with
//        static member Default = FSharp.ArbMap.defaults
//        member m.GeneratorFor<'T>() = m.ArbFor<'T>().Generator 
    


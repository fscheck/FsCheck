(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2010 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

module TypeClass =

    open System
    open System.Collections.Generic
    open System.Reflection
    
    //parametrized active pattern that recognizes generic types with generic type definitions equal to the first paramater, 
    //and that returns the generic type parameters of the generic type.
    let private (|GenericTypeDef|_|) (p:Type) (t:Type) = 
        if t.IsGenericType then
            let generic = t.GetGenericTypeDefinition() 
            if p.Equals(generic) then Some(t.GetGenericArguments()) else None
        else None  

    //returns a dictionary of generic types to methodinfo, a catch all, and array types in a list by rank
    let private findInstances (typeClass:Type) bindingFlags instancesType = 
        let addMethod ((generics,catchAll,arrays) as acc) (m:MethodInfo) =
            match m.ReturnType with
            | GenericTypeDef typeClass args when args.Length <> 1 -> 
                failwithf "Typeclasses must have exactly one generic parameter. Typeclass %A has %i" typeClass args.Length
            | GenericTypeDef typeClass args when args.[0].IsGenericParameter -> 
                generics, Some m, arrays
            | GenericTypeDef typeClass args ->
                let instance = args.[0]
                if instance.IsGenericType && (instance.GetGenericArguments() |> Array.forall (fun t -> t.IsGenericParameter)) then
                    (instance.GetGenericTypeDefinition(), m) :: generics,catchAll,arrays
                elif instance.IsArray && instance.GetElementType().IsGenericParameter then
                    generics,catchAll, (instance.GetArrayRank(),m)::arrays
                else
                    (args.[0], m) :: generics,catchAll,arrays
            | _ -> acc
        let addMethods (t:Type) =
            t.GetMethods((BindingFlags.Static ||| BindingFlags.Public ||| bindingFlags))
            |> Seq.fold addMethod ([],None,[])
        let (generics, catchAll, array) = addMethods instancesType
        if generics.Length = 0 && catchAll.IsNone && array.Length = 0 then 
            failwithf "No instances found on type %s. Check that the type is public and has public static members with the right signature." instancesType.FullName
        else
            (generics, catchAll, array)

    type TypeFullName = string

    type TypeClassComparison =
        { NewInstances : Set<TypeFullName>
          OverriddenInstances : Set<TypeFullName>
          NewCatchAll : bool
          OverriddenCatchAll : bool
        }

    type TypeClass<'TypeClass> internal(?catchAll:MethodInfo,?instances:Map<TypeFullName,MethodInfo>,?arrayInstances:Map<int,MethodInfo>) =
        let instances = defaultArg instances Map.empty
        let arrayInstances = defaultArg arrayInstances Map.empty
        let keySet map = map |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        let memo = new Dictionary<_,_>() //should fix memo bug since the memo table is re-initialized when a new registration is done
         
        member x.Class = typedefof<'TypeClass>
        member x.Instances = instances |> keySet
        member x.ArrayInstances = arrayInstances |> keySet
        member x.HasCatchAll = catchAll.IsSome

        member private x.InstancesMap = instances
        member private x.ArrayInstancesMap = arrayInstances
        member private x.CatchAll = catchAll

        ///Make a new TypeClass with only the instances registered on the given type.
        ///Note that all instances of this TypeClass will not be registered on the new TypeClass. 
        ///Use Merge in addition to achieve that, or use DiscoverAndMerge to do both.
        member x.Discover(onlyPublic,instancesType) =
            let (newInstances,catchAll,newArrayInstances) = 
                findInstances x.Class (if onlyPublic then BindingFlags.Default else BindingFlags.NonPublic)  instancesType
            let instances = 
                newInstances |> List.map (fun (k,v) -> (k.FullName,v)) |> Map.ofList
            let arrays = newArrayInstances |> Map.ofList
            match catchAll with 
            | None -> TypeClass<'TypeClass>(instances=instances, arrayInstances=arrays)
            | Some ca -> TypeClass<'TypeClass>(catchAll=ca,instances=instances,arrayInstances=arrays)

        ///Merge the instances defined by the given instances type with the ones already defined in this TypeClass.
        ///Instances defined in the given type override the instances in this TypeClass instance.
        member x.Merge(overrideWith:TypeClass<'TypeClass>) =
            let instances = overrideWith.InstancesMap |> Map.fold (fun map k v -> Map.add k v map) instances
            let arrayInstances = overrideWith.ArrayInstancesMap |> Map.fold (fun map k v -> Map.add k v map) arrayInstances
            let catchAll = match overrideWith.CatchAll with None -> catchAll | newC -> newC
            match catchAll with 
            | None -> TypeClass<'TypeClass>(instances=instances, arrayInstances=arrayInstances)
            | Some ca -> TypeClass<'TypeClass>(catchAll=ca,instances=instances,arrayInstances=arrayInstances)

        ///Discover instances for this TypeClass on the given type. Merge the result with this TypeClass,
        ///with the newly discovered instances overriding the instances on this TypeClass.
        member x.DiscoverAndMerge(onlyPublic,instancesType) =
            let newTC = x.Discover(onlyPublic, instancesType)
            x.Merge(newTC)

        ///Compares this TypeClass with the given TypeClass. Returns, respectively, the new instances, overridden instances,
        ///new array instances, overridden array instances, new catch all or overridden catchall introduced by the other TypeClass.
        member x.Compare (other:TypeClass<'TypeClass>) =
            let newInstances = other.Instances - x.Instances
            let overriddenInstances = Set.intersect other.Instances x.Instances
            let toArrayFullName rank : TypeFullName = sprintf "'T[%s]" (String.replicate (rank-1) ",")
            let newArrayInstances = other.ArrayInstances - x.ArrayInstances |> Set.map toArrayFullName
            let overriddenArrayInstances = Set.intersect other.ArrayInstances x.ArrayInstances |> Set.map toArrayFullName
            let hasNewCatchAll = (not x.HasCatchAll) && other.HasCatchAll
            let hasOverriddenCatchAll = x.HasCatchAll && other.HasCatchAll
            { NewInstances = newInstances + newArrayInstances
              OverriddenInstances = overriddenInstances + overriddenArrayInstances
              NewCatchAll = hasNewCatchAll
              OverriddenCatchAll = hasOverriddenCatchAll
            }

        member x.GetInstance =
            Common.memoizeWith memo (fun (instance:Type) ->
                let tryCatchAll instance =
                    match catchAll with
                    | Some mi   -> mi.MakeGenericMethod([|instance|])
                    | None      -> failwithf "No instances of class %A for type %A" x.Class instance
                let mi =
                    match Map.tryFind instance.FullName instances with
                    | Some res -> res
                    | _ when instance.IsGenericType -> //exact type is not known, try the generic type
                        match Map.tryFind (instance.GetGenericTypeDefinition().FullName) instances with
                        | Some mi' -> if mi'.ContainsGenericParameters then (mi'.MakeGenericMethod(instance.GetGenericArguments())) else mi'
                        | _ -> tryCatchAll instance
                    | _ when instance.IsArray ->
                        match Map.tryFind (instance.GetArrayRank()) arrayInstances with
                        | Some mi' -> if mi'.ContainsGenericParameters then mi'.MakeGenericMethod([|instance.GetElementType()|]) else mi'
                        | _ -> tryCatchAll instance
                    | _ -> tryCatchAll instance
                mi.Invoke(null, Array.empty))

        member x.InstanceFor<'T,'TypeClassT>() = x.GetInstance typeof<'T> |> unbox<'TypeClassT> //'TypeClassT = 'TypeClass<'T>
        static member New<'TypeClass>() =
            let t = (typedefof<'TypeClass>)
            if t.IsGenericTypeDefinition then TypeClass<'TypeClass>() else failwith "'TypeClass type parameter must be a generic type definition."
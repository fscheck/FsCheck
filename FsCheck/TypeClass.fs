(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2009 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

module public TypeClass

open System
open System.Collections.Generic
open System.Reflection
  
let private typeClasses = new Dictionary<_,_>() //generic types per typeClass
let private catchAlls = new Dictionary<_,_>()   //catchall types per typeClass
let private arrays = new Dictionary<_,_>()      //array types per typeClass

///Define a new typeclass of the given (generic) type. The methods on this class are the ones that will be associated with
///its generic type.
let newTypeClass<'typeClass> = 
    let t = (typeof<'typeClass>).GetGenericTypeDefinition()
    typeClasses.Add(t, new Dictionary<_,_>())
    catchAlls.Add(t, None)
    arrays.Add(t, Array.create 10 None) //index is array rank, so up to rank 10, should be enough

//parametrized active pattern that recognizes generic types with generic type definitions equal to the first paramater, 
//and that returns the generic type parameters of the generic type.
let private (|GenericTypeDef|_|) (p:Type) (t:Type) = 
    if t.IsGenericType then
        let generic = t.GetGenericTypeDefinition() 
        if p.Equals(generic) then Some(t.GetGenericArguments()) else None
    else None  

//returns a dictionary of generic types to methodinfo, a catch all, and array types in a list by rank
let private findInstances (typeClass:Type) instancesType = 
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
        t.GetMethods((BindingFlags.Static ||| BindingFlags.Public))
        |> Seq.fold addMethod ([],None,[])
    addMethods instancesType


///Register instances in a given class as instances of the given type class. 
let registerInstancesByType typeClass instance =
    let (generics, catchAll, array) = findInstances typeClass instance 
    Seq.iter (typeClasses.[typeClass].Add) generics
    if catchAll.IsSome then
        if catchAlls.[typeClass].IsSome then 
            failwithf "Catchall already registered for typeclass %A" typeClass
        else
            catchAlls.[typeClass] <- catchAll
    Seq.iter (fun (i,m) -> 
                if arrays.[typeClass].[i] = None then 
                    arrays.[typeClass].[i] <- Some m 
                else 
                    failwithf "Rank %i array type already registered for typeclass %A" i typeClass) array

///Register instances in a given class as instances of the given type class.
let registerInstances<'typeClass,'instance>() = 
    registerInstancesByType (typedefof<'typeClass>) (typeof<'instance>)

///Register instances in a given class as instances of the given type class, overwriting any existing instances. 
let overwriteInstancesByType typeClass instance =
    let (generics, catchAll, array) = findInstances typeClass instance 
    Seq.iter (fun (t,mi) -> typeClasses.[typeClass].[t] <- mi) generics
    if catchAll.IsSome then catchAlls.[typeClass] <- catchAll
    Seq.iter (fun (i,m) -> arrays.[typeClass].[i] <- Some m ) array               
    //findInstances typeClass instance |> Seq.iter (fun (t,mi) -> typeClasses.[typeClass].[t] <- mi)

///Register instances in a given class as instances of the given type class, overwriting any existing instances.  
let overwriteInstances<'typeClass,'instance>() = 
    overwriteInstancesByType (typedefof<'typeClass>) (typeof<'instance>)
    //let typeClass = typedefof<'typeClass>
    //findInstances typeClass (typeof<'instance>) |> Seq.iter (fun (t,mi) -> typeClasses.[typeClass].[t] <- mi)

///Get the typeclass instance from the given typeclass and instance type.
let getInstance =
    Common.memoize (fun (typeClass:Type, instance:Type) ->
        let instances = typeClasses.[typeClass]
        let tryCatchAll instance =
            match catchAlls.[typeClass] with
            | Some mi   -> mi.MakeGenericMethod([|instance|])
            | None      -> failwithf "No instances of class %A for type %A" typeClass instance
        let mi =
            match instances.TryGetValue(instance) with
            | (true, res) -> res
            | _ when instance.IsGenericType -> //exact type is not known, try the generic type
                match instances.TryGetValue(instance.GetGenericTypeDefinition()) with
                | (true, mi') -> if mi'.ContainsGenericParameters then (mi'.MakeGenericMethod(instance.GetGenericArguments())) else mi'
                | _ -> tryCatchAll instance
            | _ when instance.IsArray ->
                match arrays.[typeClass].[instance.GetArrayRank()] with
                | Some mi' -> if mi'.ContainsGenericParameters then mi'.MakeGenericMethod([|instance.GetElementType()|]) else mi'
                | _ -> tryCatchAll instance
            | _ -> tryCatchAll instance
        mi.Invoke(null, Array.empty))
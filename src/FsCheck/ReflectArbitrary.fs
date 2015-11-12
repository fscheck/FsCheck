﻿(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)


namespace FsCheck

module internal ReflectArbitrary =

    open System
    open System.Linq
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Reflect
    open Gen

    /// Generate a random enum of the type specified by the System.Type
    let enumOfType (t: System.Type) : Gen<Enum> =
       let isFlags = t.GetTypeInfo().GetCustomAttributes(typeof<System.FlagsAttribute>,false).Any() 
       let vals: Array = System.Enum.GetValues(t) 
       let elems = elements [ for i in 0..vals.Length-1 -> vals.GetValue(i) :?> System.Enum] 
       if isFlags then 
           let orElems (els:Enum list) = 
               let v:int = els |> List.map (box >> unbox) |> List.fold (|||) 0 
               Enum.ToObject(t,v) :?> Enum 
           listOf elems |> map orElems 
       else 
           elems

    let private reflectObjGenerator =
        Common.memoize (fun (t:Type) ->
            if isRecordType t then
                let fts = [|for pi in getRecordFields t do
                                if pi.PropertyType = t then
                                    failwithf "Recursive record types cannot be generated automatically: %A" t
                                    else yield pi.PropertyType|]
                let create = getRecordConstructor t
                fun getGenerator ->
                    let result = fts |> Seq.map getGenerator |> sequence |> map (List.toArray >> create)
                    box result

            elif isTupleType t then
                let fts = [| for elem in FSharpType.GetTupleElements t do yield elem |]
                let create elems = FSharpValue.MakeTuple (elems,t)
                fun getGenerator ->
                    let result = fts |> Seq.map getGenerator |> sequence |> map (List.toArray >> create)
                    box result

            elif isUnionType t then
                // figure out the "size" of a union
                // 0 = nullary, 1 = non-recursive, 2 = recursive
                let unionSize (ts : list<Type>) : int =
                    if ts.IsEmpty then 
                        0 
                    else
                        if List.exists(fun (x : Type) -> x.ToString() = t.ToString()) ts then 2 else 1
                        
                let unionGen create ts getGenerator =
                    let productGen (ts : list<Type>) =
                        let gs = ts |> List.map getGenerator
                        let n = gs.Length
                        [| for g in gs -> sized (fun s -> resize ((s / n) - 1) g )|]
                    let g = productGen ts
                    let res = g |> sequence |> map (List.toArray >> create)
                    res

                let gs = [| for _,(_,fields,create,_) in getUnionCases t -> unionSize fields, unionGen create fields |]
                let lowest = Array.reduce min <| Array.map fst gs
                let small getGenerator = [| for i,g in gs do if i = lowest then yield g getGenerator |]
                let large getGenerator = [| for _,g in gs -> g getGenerator |]

                fun getGenerator ->
                  let getgs size =
                      if size <= 0 then small getGenerator else large getGenerator
                      |> oneof
                      |> resize (size - 1)

                  sized getgs |> box
                
            elif t.GetTypeInfo().IsEnum then
                fun getGenerator ->
                    enumOfType t |> box

            elif isCSharpRecordType t then
                let fts = [|  for ft in getCSharpRecordFields t do
                                  if ft = t then
                                      failwithf "Recursive record types cannot be generated automatically: %A" t
                                        else yield ft|]
                let create = getCSharpRecordConstructor t
                fun getGenerator ->
                    let result = fts |> Seq.map getGenerator |> sequence |> map (List.toArray >> create)
                    box result

            else
                failwithf "The type %s is not handled automatically by FsCheck. Consider using another type or writing and registering a generator for it." t.FullName)

    let private reflectObj getGenerator t =
        let f = reflectObjGenerator t
        f getGenerator

    let private reflectGenObj getGenerator (t:Type) = (reflectObj getGenerator t |> unbox<IGen>).AsGenObject

    ///Builds a generator for the given type based on reflection. Currently works for record and union types.
    let reflectGen<'a> getGenerator = map unbox<'a> (reflectGenObj getGenerator typeof<'a>)

    let rec private children0 (seen : Set<string>) (tFind : Type) (t : Type) : (obj -> list<obj>) =
        if tFind = t then fun o -> [o]
        else children1 seen tFind t

    and private children1 (seen : Set<string>) (tFind : Type) (t : Type) =
        let ts = t.ToString();
        let seen2 = seen.Add ts
        if seen.Contains ts then
            fun _ -> []
        elif t.IsArray then
            let f = children0 seen2 tFind (t.GetElementType())
            fun o ->
                let x = o :?> Array
                List.concat [ for i in 0 .. x.Length-1 -> f (x.GetValue i) ]
        elif isUnionType t then
            let read = getUnionTagReader t
            let mp =
                Map.ofArray
                    [| for _,(tag,ts,_,destroy) in getUnionCases t ->
                        let cs = [ for u in ts -> children0 seen2 tFind u ]
                        tag, fun o -> List.concat <| List.map2 (<|) cs (List.ofArray <| destroy o)
                    |]
            fun o -> mp.[read o] o
        elif isRecordType t then
            let destroy = getRecordReader t
            let cs = [ for i in getRecordFields t -> children0 seen2 tFind i.PropertyType ]
            fun o -> List.concat <| List.map2 (<|) cs (List.ofArray <| destroy o)
        else
            fun _ -> []

    let private reflectShrinkObj getShrink o (t:Type) = 
        //assumes that l contains at least one element. 
        let split3 l =
            let rec split3' front m back =
                seq { match back with
                        | [] -> yield (front, m, back)
                        | x::xs -> yield (front,m,back); yield! split3' (front @ [m]) x xs }
            split3' [] (List.head l) (List.tail l)
        let shrinkChildren read make o childrenTypes =
            seq{ for (front,(childVal,childType),back) in Seq.zip (read o) childrenTypes |> Seq.toList |> split3 do
                    for childShrink in getShrink childType childVal do
                        yield make ( (List.map fst front) @ childShrink :: (List.map fst back) |> List.toArray)}
        if isUnionType t then
            let unionSize (t:Type) children =
                if Seq.isEmpty children then 0 
                elif Seq.exists ((=) t)(*(fun x -> x.ToString() = t.ToString())*) children then 2 
                else 1
            let info,_ = FSharpValue.GetUnionFields(o,t)
            let makeCase = FSharpValue.PreComputeUnionConstructor info
            let readCase = FSharpValue.PreComputeUnionReader info
            let childrenTypes = info.GetFields() |> Array.map ( fun x -> x.PropertyType ) //|> Array.toList
            let partitionCase t s0 (_,(_,children,make,_)) =
                match unionSize t children with
                | 0 -> (make::s0)
                | _ -> s0 
            let size0Cases() =
                getUnionCases t 
                |> List.fold (partitionCase t) []
                |> List.map (fun make -> make [||])
            //like tuple types: shrink first subtype first, then try second etc
            let shrunkChildren = shrinkChildren readCase makeCase o childrenTypes
            let children() = children1 Set.empty t t o
            match unionSize t childrenTypes with
            | 0 -> Seq.empty
            | x when x = 1 || x = 2 -> 
                seq { yield! size0Cases() 
                      yield! children()
                      yield! shrunkChildren }
            | _ -> failwith "Unxpected union size" 
        elif isRecordType t then 
            let make = getRecordConstructor t
            let read = getRecordReader t
            let childrenTypes = FSharpType.GetRecordFields t |> Array.map (fun pi -> pi.PropertyType)
            shrinkChildren read make o childrenTypes
        elif isTupleType t then
            let childrenTypes = FSharpType.GetTupleElements t
            let make = fun tuple -> FSharpValue.MakeTuple(tuple,t)
            let read = FSharpValue.GetTupleFields
            shrinkChildren read make o childrenTypes
        else
            Seq.empty

    let reflectShrink getShrink (a:'a) = reflectShrinkObj getShrink a (typeof<'a>) |> Seq.map (unbox<'a>)
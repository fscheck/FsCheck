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

module internal ReflectArbitrary

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Reflect

let private reflectObj  =
    // Compute which types are possible children of this type
    // Helps make union generation terminate quicker
    let containedTypes (t : Type) : list<Type> = [] // TODO

    //this finds the generators for each of the types, then chooses one element for each type (so, a product type like tuples)
    let productGen (ts : list<Type>) =
        let gs = [ for t in ts -> getGenerator t ]
        let n = gs.Length
        [ for g in gs -> sized (fun s -> resize ((s / n) - 1) g )]//(unbox<IGen> g).AsGenObject) ]
    
    Common.memoize (fun (t:Type) ->
        if isRecordType t then
            let g =
                productGen [ for pi in getRecordFields t do 
                                if pi.PropertyType = t then 
                                    failwithf "Recursive record types cannot be generated automatically: %A" t 
                                else yield pi.PropertyType ]
            let create = getRecordConstructor t
            let result = g |> sequence |> fmapGen (List.to_array >> create)
            box result

        elif isUnionType t then
            // figure out the "size" of a union
            // 0 = nullary, 1 = non-recursive, 2 = recursive
            let unionSize (ts : list<Type>) : int =
                if ts.IsEmpty then 0 else
                    let tsStar = List.concat (ts :: List.map containedTypes ts) //containedTypes is not implemented, always returns[]
                    if List.exists(fun (x : Type) -> x.ToString() = t.ToString()) tsStar then 2 else 1
                    
            let unionGen create ts =
                let g = productGen ts
                let res = g |> sequence |> fmapGen (List.to_array >> create)
                res

            let gs = [ for _,(_,fields,create,_) in getUnionCases t -> unionSize fields, lazy (unionGen create fields) ]
            let lowest = List.reduce_left min <| List.map fst gs
            let small() = [ for i,g in gs do if i = lowest then yield g.Force() ]
            let large() = [ for _,g in gs -> g.Force() ]
            let getgs size = 
                if size <= 0 then small() else large() 
                |> oneof 
                |> resize (size - 1) 
            sized getgs |> box
        else
            failwithf "Geneflect: type not handled %A" t)

let private reflectGenObj (t:Type) = (reflectObj t |> unbox<IGen>).AsGenObject

///Builds a generator for the given type based on reflection. Currently works for record and union types.
let reflectGen<'a> = fmapGen (unbox<'a>) (reflectGenObj (typeof<'a>))

let rec private children0 (seen : Set<string>) (tFind : Type) (t : Type) : (obj -> list<obj>) =
            if tFind = t then
                fun o -> [o]
            else
                children1 seen tFind t

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
            Map.of_array
                [| for _,(tag,ts,_,destroy) in getUnionCases t ->
                    let cs = [ for u in ts -> children0 seen2 tFind u ]
                    tag, fun o -> List.concat <| List.map2 (<|) cs (List.of_array <| destroy o)
                |]
        fun o -> mp.[read o] o
    elif isRecordType t then
        let destroy = getRecordReader t
        let cs = [ for i in getRecordFields t -> children0 seen2 tFind i.PropertyType ]
        fun o -> List.concat <| List.map2 (<|) cs (List.of_array <| destroy o)
    else
        fun _ -> []

let private reflectShrinkObj o (t:Type) = 
    //assumes that l contains at least one element. 
    let split3 l =
        let rec split3' front m back =
            seq { match back with
                    | [] -> yield (front, m, back)
                    | x::xs -> yield (front,m,back); yield! split3' (front @ [m]) x xs }
        split3' [] (List.hd l) (List.tl l)
    let shrinkChildren read make o childrenTypes =
        seq{ for (front,(childVal,childType),back) in Seq.zip (read o) childrenTypes |> Seq.to_list |> split3 do
                        for childShrink in getShrink childType childVal do
                            yield make ( (List.map fst front) @ childShrink :: (List.map fst back) |> List.to_array)}
    if isUnionType t then
        let unionSize (t:Type) children =
            if Seq.is_empty children then 0 
            elif Seq.exists ((=) t)(*(fun x -> x.ToString() = t.ToString())*) children then 2 
            else 1
        let info,vals = FSharpValue.GetUnionFields(o,t)
        let makeCase = FSharpValue.PrecomputeUnionConstructor info
        let readCase = FSharpValue.PrecomputeUnionReader info
        let childrenTypes = info.GetFields() |> Array.map ( fun x -> x.PropertyType ) //|> Array.to_list
        let partitionCase t s0 (_,(_,children,make,_)) =
            match unionSize t children with
            | 0 -> (make::s0)
            | _ -> s0 
        let size0Cases() =
            getUnionCases t 
            |> List.fold_left (partitionCase t) []
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
    else
        Seq.empty

let reflectShrink (a:'a) = reflectShrinkObj a (typeof<'a>) |> Seq.map (unbox<'a>)
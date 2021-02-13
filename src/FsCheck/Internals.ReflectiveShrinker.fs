namespace FsCheck.Internals

open System
open System.Linq
open System.Reflection

open Microsoft.FSharp.Reflection

open FsCheck.Internals.Reflect

module internal ReflectiveShrinker =
    let rec private children0 (seen:Set<string>) (tFind:Type) (t:Type) : (obj -> list<obj>) =
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
            let read = getRecordReader t
            let cs = [ for fieldType in getRecordFieldTypes t -> children0 seen2 tFind fieldType ]
            fun o -> List.concat <| List.map2 (<|) cs (List.ofArray <| read o)
        else
            fun _ -> []

    let private reflectShrinkObj getShrink o (t:Type) = 
        //assumes that l contains at least one element. 
        let split3 l =
            let rec split3' front m back =
                seq { match back with
                      | [] -> yield (front, m, back)
                      | x::xs -> yield (front, m, back)
                                 yield! split3' (front @ [m]) x xs }
            split3' [] (List.head l) (List.tail l)
        let shrinkChildren read make o childrenTypes =
            seq{ for (front,(childVal,childType),back) in Seq.zip (read o) childrenTypes |> Seq.toList |> split3 do
                    for childShrink in getShrink childType childVal do
                        yield make ( (List.map fst front) @ childShrink :: (List.map fst back) |> List.toArray)}

        if isUnionType t then
            let unionSize (t:Type) children =
                if Seq.isEmpty children then 0 
                elif Seq.exists ((=) t) children then 2 
                else 1
            let info,_ = FSharpValue.GetUnionFields(o,t)
            let makeCase = FSharpValue.PreComputeUnionConstructor info
            let readCase = FSharpValue.PreComputeUnionReader info
            let childrenTypes = info.GetFields() |> Array.map ( fun x -> x.PropertyType )
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
            | _ -> failwith "Unexpected union size" 

        elif isRecordType t then 
            let make = getRecordConstructor t
            let read = getRecordReader t
            let childrenTypes = getRecordFieldTypes t
            shrinkChildren read make o childrenTypes

        elif isTupleType t then
            let childrenTypes = FSharpType.GetTupleElements t
            let make = fun tuple -> FSharpValue.MakeTuple(tuple,t)
            let read = FSharpValue.GetTupleFields
            shrinkChildren read make o childrenTypes

        elif isCSharpDtoType t then
            let make = getCSharpDtoConstructor t
            let read = getCSharpDtoReader t
            let childrenTypes = getCSharpDtoFields t
            shrinkChildren read make o childrenTypes

        elif t.GetTypeInfo().IsEnum then
            let isFlags = t.GetTypeInfo().GetCustomAttributes(typeof<FlagsAttribute>,false).Any() 
            if isFlags then
                let vals: Array = Enum.GetValues(t)
                let elems = [ for i in 0..vals.Length-1 -> vals.GetValue(i) ]
                let elementType = Enum.GetUnderlyingType t
                let n = Convert.ChangeType(o, elementType)
                let inline helper (e : 'a) =
                    seq {
                        for i in elems do
                            let _i = unbox<'a> i
                            if Common.isPowerOf2 _i then
                                let withoutFlag = e &&& (~~~ _i)
                                if (withoutFlag <> e) then yield withoutFlag :> obj
                    }
                    |> Seq.distinct
                if   elementType = typeof<byte>   then helper (unbox<byte> n)
                elif elementType = typeof<sbyte>  then helper (unbox<sbyte> n)
                elif elementType = typeof<uint16> then helper (unbox<uint16> n)
                elif elementType = typeof<int16>  then helper (unbox<int16> n)
                elif elementType = typeof<uint32> then helper (unbox<uint32> n)
                elif elementType = typeof<int>    then helper (unbox<int> n)
                elif elementType = typeof<uint64> then helper (unbox<uint64> n)
                elif elementType = typeof<int64>  then helper (unbox<int64> n)
                else invalidArg "t" (sprintf "Unexpected underlying enum type: %O" elementType)                    
            else
                Seq.empty

        else
            Seq.empty

    let reflectShrink getShrink (a:'a) = reflectShrinkObj getShrink a (typeof<'a>) |> Seq.map (unbox<'a>)


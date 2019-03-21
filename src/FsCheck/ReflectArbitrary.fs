namespace FsCheck

module internal ReflectArbitrary =

    open System
    open System.Linq
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Reflect
    open Gen

    let inline isPowerOf2 n =
        (n <> LanguagePrimitives.GenericZero) && 
        ((n &&& (n - LanguagePrimitives.GenericOne)) = LanguagePrimitives.GenericZero) 

    /// Generate a random enum of the type specified by the System.Type
    let enumOfType (t: System.Type) : Gen<Enum> =
       let isFlags = t.GetTypeInfo().GetCustomAttributes(typeof<System.FlagsAttribute>,false).Any() 
       let vals: Array = System.Enum.GetValues(t)
       let elems = [ for i in 0..vals.Length-1 -> vals.GetValue(i)] |> List.distinct  
       if isFlags then
           let elementType = System.Enum.GetUnderlyingType t
           let inline helper (elements : 'a list) =
               let primaries = elements |> List.filter isPowerOf2
               Gen.elements [true; false]
               |> Gen.listOfLength primaries.Length
               |> Gen.map (
                   fun bools ->
                       bools
                       |> List.map2 
                           (fun flag isChecked -> if isChecked then flag else LanguagePrimitives.GenericZero )
                           primaries    
                       |> List.fold (|||) LanguagePrimitives.GenericZero )
               |> Gen.map (fun e -> Enum.ToObject (t, e) :?> Enum)
           if   elementType = typeof<byte>   then
               elems |> List.map unbox<byte>   |> helper
           elif elementType = typeof<sbyte>  then 
               elems |> List.map unbox<sbyte>  |> helper
           elif elementType = typeof<uint16> then
               elems |> List.map unbox<uint16> |> helper
           elif elementType = typeof<int16>  then
               elems |> List.map unbox<int16>  |> helper
           elif elementType = typeof<uint32> then
               elems |> List.map unbox<uint32> |> helper
           elif elementType = typeof<int>    then
               elems |> List.map unbox<int>    |> helper
           elif elementType = typeof<uint64> then 
               elems |> List.map unbox<uint64> |> helper
           elif elementType = typeof<int64>  then
               elems |> List.map unbox<int64>  |> helper
           else invalidArg "t" (sprintf "Unexpected underlying enum type: %O" elementType)
       else 
           Gen.elements (List.map (fun (o : obj) -> o :?> Enum) elems)

    let private reflectObj getGenerator t =

        // is there a path via the fieldType back to the containingType?
        let rec isRecursive (fieldType:Type) (containingType:Type) (seen:Set<string>) =
            let children t =
                if isRecordType t then
                    getRecordFieldTypes t :> seq<_>
                elif isUnionType t then
                    FSharpType.GetUnionCases(t, true) 
                    |> Seq.collect (fun uc -> uc.GetFields() |> Seq.map (fun pi -> pi.PropertyType))
                else
                    Seq.empty

            fieldType = containingType
            || (if seen.Contains(fieldType.AssemblyQualifiedName) then 
                    false
                else
                    let fields = children fieldType
                    let newSeen = seen.Add fieldType.AssemblyQualifiedName
                    fields |> Seq.exists (fun field -> isRecursive field containingType newSeen))

        let productGen (ts : seq<Type>) create =
            let gs = [| for t in ts -> getGenerator t |]
            let n = gs.Length
            if n <= 0 then
                Gen.constant (create [||])
            else
                sized (fun s -> resize (max 0 ((s / n) - 1)) (sequenceToArr gs))
                |> map create

        if isRecordType t then
            let fields = getRecordFieldTypes t
            if fields |> Seq.exists ((=) t) then 
                failwithf "Recursive record types cannot be generated automatically: %A" t 
            let create = getRecordConstructor t
            let g = productGen fields create
            box g

        elif isTupleType t then
            let fields = FSharpType.GetTupleElements t
            let create elems = FSharpValue.MakeTuple (elems,t)
            let g = productGen fields create
            box g

        elif isUnionType t then
            // figure out the "size" of a union
            // 0 = nullary, 1 = non-recursive, 2 = recursive
            let unionSize (ts : list<Type>) : int =
                if ts.IsEmpty then 0 
                elif List.exists(fun x -> isRecursive x t Set.empty) ts then 2
                else 1

            let gs = [ for _,(_,fields,create,_) in getUnionCases t -> unionSize fields, lazy (productGen fields create) ]
            let lowest = List.reduce min <| List.map fst gs
            let small() = [ for i,g in gs do if i = lowest then yield g.Force() ]
            let large() = [ for _,g in gs -> g.Force() ]
            let getgs size = 
                if size <= 0 then small() else large() 
                |> oneof 
                |> resize (size - 1) 
            sized getgs |> box
                
        elif t.GetTypeInfo().IsEnum then
            enumOfType t |> box

        elif isCSharpRecordType t then
            let fields = getCSharpRecordFields t
            if fields |> Seq.exists ((=) t) then 
                failwithf "Recursive record types cannot be generated automatically: %A" t 
            let create = getCSharpRecordConstructor t
            let g = productGen fields create
            box g

        elif isCSharpDtoType t then
            let fields = getCSharpDtoFields t
            if fields |> Seq.exists ((=) t) then 
                failwithf "Recursive record types cannot be generated automatically: %A" t 
            let create = getCSharpDtoConstructor t
            let g = productGen fields create
            box g

        else
            failwithf "The type %s is not handled automatically by FsCheck. Consider using another type or writing and registering a generator for it." t.FullName
                
    ///Build a reflection-based generator for the given Type. Since we memoize based on type, can't use a
    ///typed variant reflectGen<'a> much here, as we need to be able to partially apply on the getGenerator.
    ///See also Default.Derive.
    let reflectGenObj getGenerator = Common.memoize (fun (t:Type) ->(reflectObj getGenerator t |> unbox<IGen>).AsGenObject)

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
            | _ -> failwith "Unxpected union size" 

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
            let isFlags = t.GetTypeInfo().GetCustomAttributes(typeof<System.FlagsAttribute>,false).Any() 
            if isFlags then
                let vals: Array = System.Enum.GetValues(t)
                let elems = [ for i in 0..vals.Length-1 -> vals.GetValue(i) ]
                let elementType = System.Enum.GetUnderlyingType t
                let n = Convert.ChangeType(o, elementType)
                let inline helper (e : 'a) =
                    seq {
                        for i in elems do
                            let _i = unbox<'a> i
                            if isPowerOf2 _i then
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
namespace FsCheck.Internals

module internal ReflectiveGenerator =

    open System
    open System.Linq
    open System.Reflection

    open Microsoft.FSharp.Reflection
    
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Internals
    open FsCheck.Internals.Reflect



    /// Generate a random enum of the type specified by the System.Type
    let enumOfType (t: System.Type) : Gen<Enum> =
       let isFlags = t.GetTypeInfo().GetCustomAttributes(typeof<FlagsAttribute>,false).Any() 
       let vals: Array = Enum.GetValues(t)
       let elems = [ for i in 0..vals.Length-1 -> vals.GetValue(i)] |> List.distinct  
       if isFlags then
           let elementType = Enum.GetUnderlyingType t
           let inline helper (elements : 'a list) =
               let primaries = elements |> List.filter Common.isPowerOf2
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
                Gen.sized (fun s -> Gen.resize (max 0 ((s / n) - 1)) (Gen.sequenceToArray gs))
                |> Gen.map create

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
                |> Gen.oneof 
                |> Gen.resize (size - 1) 
            Gen.sized getgs |> box
                
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

    
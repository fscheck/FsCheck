namespace FsCheck

///Haskell typeclass concept simulation. For internal use.
module TypeClass =

    open System
    open System.Collections.Generic
    open System.Reflection

    open Common
    
    //parametrized active pattern that recognizes generic types with generic type definitions equal to the first paramater, 
    //and that returns the generic type parameters of the generic type.
    let private (|GenericTypeDef|_|) (p:Type) (t:Type) = 
        let tdef = t.GetTypeInfo()
        if tdef.IsGenericType then
            let generic = tdef.GetGenericTypeDefinition() 
            if p.Equals(generic) then Some(tdef.GenericTypeArguments) else None
        else None

    let private (|IsArray|IsGeneric|IsOther|) (x:Type) = 
        if x.IsArray then IsArray
        elif x.GetTypeInfo().IsGenericType then IsGeneric
        else IsOther

    let [<Literal>] internal CatchAllName = "--FsCheck.CatchAll--"

    [<CustomComparison;CustomEquality>]
    type InstanceKind =
        | Primitive of Type
        | Generic of Type
        | Array of Type
        | CatchAll of Type
        static member StringStamp instanceKind =
            match instanceKind with
            | Primitive t
            | Generic t -> t.FullName
            | Array t -> sprintf "'T[%s]" (String.replicate ((t.GetArrayRank())-1) ",")
            | CatchAll _ -> CatchAllName
        override x.Equals y = equalsOn InstanceKind.StringStamp x y
        override x.GetHashCode() = hashOn InstanceKind.StringStamp x
        interface System.IComparable with
            member x.CompareTo y = compareOn InstanceKind.StringStamp x y
        static member FromType (``type``:Type) = 
            match ``type`` with
            | catchAll when catchAll.IsGenericParameter -> 
                CatchAll catchAll
            | generic when generic.GetTypeInfo().IsGenericType && (generic.GenericTypeArguments |> Array.forall (fun t -> t.IsGenericParameter)) ->
                    Generic <| generic.GetGenericTypeDefinition()
            | arr when arr.IsArray && arr.GetElementType().IsGenericParameter ->
                    Array <| arr
            | prim -> Primitive prim
 
    let private getMethods (t: Type) =
        #if NETSTANDARD1_0
        t.GetRuntimeMethods()
        #else
        t.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.FlattenHierarchy ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
        #endif
    
    //returns a dictionary of generic types to methodinfo, a catch all, and array types in a list by rank
    let private findInstances (typeClass:Type) onlyPublic instancesType = 
        let addMethod acc (m:MethodInfo) =
            match m.ReturnType with
            | GenericTypeDef typeClass args when args.Length <> 1 -> 
                failwithf "Typeclasses must have exactly one generic parameter. Typeclass %A has %i" typeClass args.Length
            | GenericTypeDef typeClass args ->
                let instance = args.[0]
                (InstanceKind.FromType instance,m) :: acc
            | _ -> acc
        let addMethods (t:Type) =
            t
            |> getMethods
            |> Seq.append (t.GetRuntimeProperties() |> Seq.where (fun prop -> prop.CanRead)|> Seq.map (fun prop -> prop.GetMethod))
            |> Seq.where(fun meth -> meth.IsStatic && (meth.IsPublic || not onlyPublic) && meth.GetParameters().Length = 0)
            |> Seq.fold addMethod []
        let instances = addMethods instancesType
        if instances.Length = 0 then 
            failwithf "No instances found on type %s. Check that the type is public and has public static members with the right signature." instancesType.FullName
        else
            instances

    [<StructuredFormatDisplay("{ToStructuredDisplay}")>]
    type TypeClassComparison =
        { NewInstances : Set<InstanceKind>
          OverriddenInstances : Set<InstanceKind>
        } with
        member x.ToStructuredDisplay = x.ToString()
        override x.ToString() =
            let setToString pre s = 
                if Set.isEmpty s then
                    ""
                else
                    (Set.fold (sprintf "%s, %A") pre s) + "\n"
            let instances = sprintf "%s%s" (setToString "New: " x.NewInstances) (setToString "Overridden: " x.OverriddenInstances)
            instances

    type TypeClass<'TypeClass> 
        internal(?instances:Map<InstanceKind,MethodInfo>) =

        let instances = defaultArg instances Map.empty
        let keySet map = map |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        let memo = new Dictionary<_,_>() //should fix memo bug since the memo table is re-initialized when a new registration is done
         
        member __.Class = typedefof<'TypeClass>
        member __.Instances = instances |> keySet
        member x.HasCatchAll = x.Instances |> Set.contains (CatchAll typeof<int> (* doesn't matter *))

        member private __.InstancesMap = instances

        ///Make a new TypeClass with only the instances registered on the given type.
        ///Note that the instances of this TypeClass will not be registered on the new TypeClass. 
        ///Use Merge in addition to achieve that, or use DiscoverAndMerge to do both.
        member x.Discover(onlyPublic,instancesType) =
            let newInstances = 
                findInstances x.Class onlyPublic instancesType
                |> Map.ofSeq
            new TypeClass<'TypeClass>(newInstances)

        ///Merge the instances defined by the given instances type with the ones already defined in this TypeClass.
        ///Instances defined in the given type override the instances in this TypeClass instance.
        member __.Merge(overrideWith:TypeClass<'TypeClass>) =
            new TypeClass<'TypeClass>(overrideWith.InstancesMap |> Map.fold (fun map k v -> Map.add k v map) instances)

        ///Discover instances for this TypeClass on the given type. Merge the result with this TypeClass,
        ///with the newly discovered instances overriding the instances on this TypeClass.
        member this.DiscoverAndMerge(onlyPublic,instancesType) =
            let newTC = this.Discover(onlyPublic, instancesType)
            this.Merge(newTC)

        ///Compares this TypeClass with the given TypeClass. Returns, respectively, the new instances, overridden instances,
        ///new array instances, overridden array instances, new catch all or overridden catchall introduced by the other TypeClass.
        member x.Compare (other:TypeClass<'TypeClass>) =
            let newInstances = other.Instances - x.Instances
            let overriddenInstances = Set.intersect other.Instances x.Instances
            { NewInstances = newInstances
              OverriddenInstances = overriddenInstances
            }

        ///Get the instance registered on this TypeClass for the given type and optionally the given arguments. 
        ///The result is of type 'TypeClass<'T>, dynamically.
        member x.GetInstance (instance:Type) =
                
            Common.memoizeWith memo (fun (instance:Type) -> 
                let mi =
                    match instance,instances with
                    | (_,MapContains (Primitive instance) mi') -> 
                        mi'
                    | (IsGeneric,MapContains (Generic (instance.GetGenericTypeDefinition())) mi') -> 
                        if mi'.ContainsGenericParameters then (mi'.MakeGenericMethod(instance.GetTypeInfo().GenericTypeArguments)) else mi'
                    | (IsArray, MapContains (Array instance) mi') -> 
                        if mi'.ContainsGenericParameters then mi'.MakeGenericMethod([|instance.GetElementType()|]) else mi'
                    | (_,MapContains (CatchAll instance) mi') ->
                        mi'.MakeGenericMethod([|instance|])
                    | _ -> failwithf "No instances of class %A for type %A" x.Class instance
                mi.Invoke(null, [||])) instance

        ///Get the instance registered on this TypeClass for the given type parameter 'T. The result will be cast
        ///to TypeClassT, which should be 'TypeClass<'T> but that's impossible to express in .NET's type system.
        member x.InstanceFor<'T,'TypeClassT>() = 
            x.GetInstance(typeof<'T>) 
            |> unbox<'TypeClassT> 

        static member New<'TypeClass>() =
            let t = (typedefof<'TypeClass>)
            if t.GetTypeInfo().IsGenericTypeDefinition then 
                TypeClass<'TypeClass>() 
            else 
                failwithf "Type parameter %s must be a generic type definition." typeof<'TypeClass>.FullName
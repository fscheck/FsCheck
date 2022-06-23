namespace FsCheck.Internals

///Haskell typeclass concept simulation. For internal use.
module internal TypeClass =

    open System
    open System.Collections.Generic
    open System.Reflection

    open FsCheck.Internals.Common
    
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

    type InvocationData = {
        Method : MethodInfo
        Target : obj
    }
 
    let private getMethods (t: Type) : seq<MethodInfo> =
        #if NETSTANDARD1_0
        t.GetRuntimeMethods()
        #else
        upcast t.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.FlattenHierarchy ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
        #endif

    let private toRegistryPair typeClass (inv: InvocationData) =
        match inv.Method.ReturnType with
        | GenericTypeDef typeClass args when args.Length <> 1 -> 
            failwithf "Typeclasses must have exactly one generic parameter. Typeclass %A has %i" typeClass args.Length
        | GenericTypeDef typeClass args ->
            let instance = args.[0]
            Some (InstanceKind.FromType instance, inv)
        | _ -> None

    type ParameterErrors = 
        | NoneAllowed
        | NotCreatable of ParameterInfo array

    module Result = 
        let isOk = function | Ok _ -> true | _ -> false

    let private validateParameters (typeClass:Type) (injectParameters: bool) injectedConfigTypes (meth:MethodInfo) =
        let isUsableMethodArgumentType argumentType =
            match argumentType with
            | GenericTypeDef typeClass args when args.Length <> 1 -> 
                failwithf "Typeclasses must have exactly one generic parameter. Typeclass %A has %i" typeClass args.Length
            | GenericTypeDef typeClass _  ->
                true
            | t when injectedConfigTypes |> Array.exists t.IsAssignableFrom ->
                true
            | _ ->
                false

        if injectParameters then
            let invalidParameters = 
                meth.GetParameters()
                |> Array.filter (fun p -> not(isUsableMethodArgumentType p.ParameterType))
            if Array.isEmpty invalidParameters
            then Ok ()
            else Error  (NotCreatable invalidParameters)
        else
            if meth.GetParameters().Length = 0
            then Ok ()
            else Error (NoneAllowed)
    
    
    //returns a dictionary of generic types to methodinfo, a catch all, and array types in a list by rank
    let private findInstances (typeClass:Type) onlyPublic injectParameters injectedConfigs instancesType (instance: _ option) = 
        let filterInstanceOrStatic (meth:MethodInfo) =
            if instance.IsSome then
                not meth.IsStatic
            else
                meth.IsStatic
        let filterVisibility (meth:MethodInfo) =
             meth.IsPublic || not onlyPublic

        let injectedConfigTypes = injectedConfigs |> Array.map (fun e -> e.GetType())
        let filterParameters (meth:MethodInfo) = 
            validateParameters typeClass injectParameters injectedConfigTypes meth
            |> Result.isOk

        let addMethod acc (m:MethodInfo) =
            match toRegistryPair typeClass { Target = null; Method = m } with
            | Some registration -> registration :: acc
            | None -> acc
        let addMethods (t:Type) =
            t
            |> getMethods
            |> Seq.append (t.GetRuntimeProperties() |> Seq.where (fun prop -> prop.CanRead)|> Seq.map (fun prop -> prop.GetMethod))
            |> Seq.where(fun meth -> filterInstanceOrStatic(meth) && filterVisibility(meth) && filterParameters(meth))
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
        internal(?instances:Map<InstanceKind,InvocationData>, ?injectParameters:bool, ?injectedConfigs:array<obj>) =

        let instances = defaultArg instances Map.empty
        let keySet map = map |> Map.toSeq |> Seq.map fst |> Set.ofSeq

        let memo = new Dictionary<_,_>() //should fix memo bug since the memo table is re-initialized when a new registration is done
         
        let injectParameters = defaultArg injectParameters false 
        let injectedConfigs = defaultArg injectedConfigs [||]
        let injectedConfigByType = injectedConfigs |> Array.map (fun e -> e.GetType(),e)

        member __.Class = typedefof<'TypeClass>
        member __.Instances = instances |> keySet
        member x.HasCatchAll = x.Instances |> Set.contains (CatchAll typeof<int> (* doesn't matter *))

        member private _.InstancesMap = instances
        member private _.InjectedConfigs = injectedConfigs

        // for testing purposes
        member internal _.MemoizedInstances = memo.Keys |> Seq.cast<Type> |> Array.ofSeq

        ///Make a new TypeClass with only the instances registered on the given type.
        ///Note that the instances of this TypeClass will not be registered on the new TypeClass. 
        ///Use Merge in addition to achieve that, or use DiscoverAndMerge to do both.
        /// The newInjectedConfigs are prepended to the injectedConfigs in this class;
        /// this ensures they take precendence if there are existing configs of the same type.
        member x.Discover(onlyPublic,instancesType, ?newInjectedConfigs) =
            let newInjectedConfigs = Array.append  (defaultArg newInjectedConfigs [||]) injectedConfigs
            let newInstances = 
                findInstances x.Class onlyPublic injectParameters newInjectedConfigs instancesType None
                |> Map.ofSeq
            new TypeClass<'TypeClass>(newInstances, injectParameters, newInjectedConfigs)

        ///Make a new TypeClass with only the instances registered on the given instance.
        ///Note that the instances of this TypeClass will not be registered on the new TypeClass. 
        ///Use Merge in addition to achieve that, or use DiscoverAndMerge to do both.
        member x.Discover<'T>(onlyPublic,instance:'T) =
            let newInstances = 
                findInstances x.Class onlyPublic injectParameters injectedConfigs typeof<'T> (Some instance)
                |> Map.ofSeq
            new TypeClass<'TypeClass>(newInstances, injectParameters, injectedConfigs)

        ///Merge the instances defined by the given instances type with the ones already defined in this TypeClass.
        ///Instances defined in the given type override the instances in this TypeClass instance.
        member __.Merge(overrideWith:TypeClass<'TypeClass>) =
            new TypeClass<'TypeClass>(overrideWith.InstancesMap |> Map.fold (fun map k v -> Map.add k v map) instances, injectParameters, overrideWith.InjectedConfigs)

        ///Discover instances for this TypeClass on the given type. Merge the result with this TypeClass,
        ///with the newly discovered instances overriding the instances on this TypeClass.
        member this.DiscoverAndMerge(onlyPublic,instancesType:Type, ?newInjectedConfigs: obj[]) =
            let newTC = this.Discover(onlyPublic, instancesType, ?newInjectedConfigs=newInjectedConfigs)
            this.Merge(newTC)

        ///Discover instances for this TypeClass on the given type. Merge the result with this TypeClass,
        ///with the newly discovered instances overriding the instances on this TypeClass.
        member this.DiscoverAndMerge<'T>(onlyPublic,instance:'T) =
            let newTC = this.Discover<'T>(onlyPublic, instance)
            this.Merge(newTC)

        /// Merge a single instance factory with the existing TypeClass, overriding existing instances on this TypeClass
        /// Factory must return an instance of the given TypeClass.
        member this.MergeFactory(factory: Func<'a,'b>) =
            match box factory with
            | :? Func<unit, 'b> as f -> 
                let factory = Func<'b>(f.Invoke)
                this.MergeFactory(factory.Target, factory.Method)
            | _ -> this.MergeFactory(factory.Target, factory.Method) 

        member internal this.MergeFactory(target: obj, method: MethodInfo) = 
            let factory = { Target = target; Method = method}
            let toRegistryPair' (inv: InvocationData) =
                match toRegistryPair this.Class inv with
                | Some registration -> registration
                | None -> invalidArg "factory" "Factory did not return a compatible type for this type class"

            let throwParameterError err = 
                match err with
                | NoneAllowed -> invalidArg "factory" "Typeclass does not allow factories with parameters"
                | NotCreatable invalidParams -> 
                    let formatParam (pi:ParameterInfo) = sprintf "%s %s" pi.ParameterType.Name pi.Name
                    let paramString = invalidParams |> Array.map formatParam |> String.concat "; "
                    invalidArg "factory" (sprintf "Typeclass cannot create values for the factory's parameters. Invalid parameters: [%s]" paramString)
            let errorIfParametersUnsupported method =
                let injectedConfigTypes = injectedConfigs |> Array.map (fun e -> e.GetType())
                match validateParameters this.Class injectParameters injectedConfigTypes factory.Method with
                | Error err -> throwParameterError err
                | Ok _ -> ()


            errorIfParametersUnsupported factory.Method
            let updatedMap = this.InstancesMap.Add(toRegistryPair' factory)
            new TypeClass<'TypeClass>(updatedMap, injectParameters, injectedConfigs)
            

        ///Compares this TypeClass with the given TypeClass. Returns, respectively, the new instances, overridden instances,
        ///new array instances, overridden array instances, new catch all or overridden catchall introduced by the other TypeClass.
        member x.Compare (other:TypeClass<'TypeClass>) =
            let newInstances = other.Instances - x.Instances
            let overriddenInstances = Set.intersect other.Instances x.Instances
            { NewInstances = newInstances
              OverriddenInstances = overriddenInstances
            }
            

        ///Get the instance registered on this TypeClass for the given type. 
        ///The result is of type 'TypeClass<'T>, dynamically.
        member x.GetInstance (instance:Type) =
                
            let resolveParameter (param:ParameterInfo) =
                let maybeConfig = injectedConfigByType |> Array.tryFind (fun (t, o) -> param.ParameterType.IsAssignableFrom(t)) |> Option.map snd
                if maybeConfig.IsSome then
                    maybeConfig.Value
                else
                    match param.ParameterType with
                    | GenericTypeDef x.Class args when args.Length = 1 ->
                        x.GetInstance args.[0]
                    | _ -> failwithf "Can't inject parameter %s of method %s.%s" param.Name param.Member.DeclaringType.Name param.Member.Name
                    
                

            let invoke (inv:InvocationData) =
                let mi = inv.Method
                let parameters = 
                    mi.GetParameters()
                    |> Array.map resolveParameter
                mi.Invoke(inv.Target, parameters)

            let rec binding (concrete:Type) (generic:Type) =
                if concrete.IsGenericType && generic.IsGenericType then
                    let c = concrete.GetGenericTypeDefinition()
                    let g = generic.GetGenericTypeDefinition()
                    if c = g then
                        let cargs = concrete.GetGenericArguments()
                        let gargs = generic.GetGenericArguments()
                        Array.zip cargs gargs
                        |> Array.collect (fun (c,g) ->  binding c g)
                    else
                        failwithf "Problem when determining binding of generic type arguments: %A and %A are not the same generic type definition" concrete generic
                elif generic.IsGenericParameter then
                    [| (generic,concrete) |]
                else
                    failwithf "Unexpected case while determining binding of types:  %A and %A" concrete generic



            Common.memoizeWith memo (fun (instance:Type) -> 
                let makeGenericInvocation inv types = 
                    { inv with Method = inv.Method.MakeGenericMethod(types) }
                let inv =
                    match instance,instances with
                    | (_,MapContains (Primitive instance) inv') -> 
                        inv'
                    | (IsGeneric,MapContains (Generic (instance.GetGenericTypeDefinition())) inv') -> 
                        let mi' = inv'.Method
                        if mi'.ContainsGenericParameters then
                            let b = binding instance (mi'.ReturnType.GetGenericArguments().[0])
                            let typeArgs = mi'.GetGenericArguments() |> Array.map (fun t -> b |> Array.find (fun (l,r) -> l = t) |> snd)
                            makeGenericInvocation inv' typeArgs
                        else inv'
                    | (IsArray, MapContains (Array instance) inv') -> 
                        if inv'.Method.ContainsGenericParameters 
                        then makeGenericInvocation inv' [|instance.GetElementType()|]
                        else inv'
                    | (_,MapContains (CatchAll instance) inv') ->
                        makeGenericInvocation inv' [|instance|]
                    | _ -> failwithf "No instances of class %A for type %A" x.Class instance

                invoke inv) instance

        ///Get the instance registered on this TypeClass for the given type parameter 'T. The result will be cast
        ///to TypeClassT, which should be 'TypeClass<'T> but that's impossible to express in .NET's type system.
        member x.InstanceFor<'T,'TypeClassT>() = 
            x.GetInstance(typeof<'T>) 
            |> unbox<'TypeClassT> 

        static member New<'TypeClass>(?injectParameters) =
            let t = (typedefof<'TypeClass>)
            if t.GetTypeInfo().IsGenericTypeDefinition then 
                TypeClass<'TypeClass>(?injectParameters=injectParameters) 
            else 
                failwithf "Type parameter %s must be a generic type definition." typeof<'TypeClass>.FullName
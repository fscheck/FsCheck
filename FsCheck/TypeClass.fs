#light

namespace FsCheck

module public TypeClass =

    open System
    open System.Collections.Generic
    open System.Reflection
      
    let private typeClasses = new Dictionary<_,_>()
    let private catchAll = ref None

    ///Define a new typeclass of the given (generic) type. The methods on this class are the ones that will be associated with
    ///its generic type.
    let newTypeClass<'typeClass> = typeClasses.Add((typeof<'typeClass>).GetGenericTypeDefinition(), new Dictionary<_,_>())

    //parametrized active pattern that recognizes generic types with generic type definitions equal to the first paramater, 
    //and that returns the generic type parameters of the generic type.
    let private (|GenericTypeDef|_|) (p:Type) (t:Type) = 
        try
            let generic = t.GetGenericTypeDefinition() 
            if p.Equals(generic) then Some(t.GetGenericArguments()) else None
        with _ -> None

    let private findInstances (typeClass:Type) = 
        let addMethods l (t:Type) =
            t.GetMethods((BindingFlags.Static ||| BindingFlags.Public)) |>
            Seq.fold (fun l m ->
                match m.ReturnType with
                    | GenericTypeDef typeClass args -> 
                        if (args.Length <> 1) then
                            failwithf "Typeclasses must have exactly one generic parameter. Typeclass %A has %i" typeClass args.Length
                        elif args.[0].IsGenericParameter then
                            //found a catchall. Register it separately.
                            catchAll := Some m
                            l
                        else
                            let instance = args.[0]
                            if instance.IsGenericType 
                                && (instance.GetGenericArguments() |> Array.for_all (fun t -> t.IsGenericParameter)) then
                                (args.[0].GetGenericTypeDefinition(), m) :: l   
                            else
                                (args.[0], m) :: l
                    | _ -> l
                ) l
        addMethods []


    ///Register instances in a given class as instances of the given type class.
    let registerInstances<'typeClass,'instance>() = 
        let typeClass = typedefof<'typeClass>
        findInstances typeClass (typeof<'instance>) |> Seq.iter (typeClasses.[typeClass].Add)
    
    ///Register instances in a given class as instances of the given type class, overwriting any existing instances.  
    let overwriteInstances<'typeClass,'instance>() = 
        let typeClass = typedefof<'typeClass>
        findInstances typeClass (typeof<'instance>) |> Seq.iter (fun (t,mi) -> typeClasses.[typeClass].[t] <- mi)

    let getInstance (typeClass:Type) (instance:Type)  =
        let instances = typeClasses.[typeClass]
        let tryCatchAll instance =
            match !catchAll with
            | Some mi   -> mi.MakeGenericMethod([|instance|])
            | None      -> failwithf "No instances of class %A for type %A" typeClass instance
        let mi =
            match instances.TryGetValue(instance) with
            | (true, res) -> res
            | _ when instance.IsGenericType -> //exact type is not known, try the generic type
                match instances.TryGetValue(instance.GetGenericTypeDefinition()) with
                | (true, mi') -> if mi'.ContainsGenericParameters then (mi'.MakeGenericMethod(instance.GetGenericArguments())) else mi'
                | _ -> tryCatchAll instance
            | _ -> tryCatchAll instance
        mi.Invoke(null, Array.empty)

    //attempt to add some type safety
    //let inline arbitraryUnit(c:^c) = (^c : (static member Unit: unit -> IArbitrary<unit>) ()) |>ignore
    //let inline arbitraryOption(c:^c) = (^c : (static member Option: unit -> IArbitrary<option<'a>>) ()) |>ignore
    //let res = arbitraryUnit(new ArbitraryInstances()); arbitraryOption(new ArbitraryInstances())
        //let x = (^b : (static member ToProperty: ^a -> Property) (bOrp))
        //let x = Property.ToProperty(bOrp)//(^b : (static member ToProperty: ^a -> Property) (bOrp))
        //x


    //---------------------------------------------------------------------------------
    //doesn't work like below, since then a new type variable can't be instantiated       
    //let instance (instance:Type) (i:'a) =
    //    let typeClass = instance.GetGenericTypeDefinition()
    //    let instanceType = instance.GetGenericArguments().[0]
    //    let actual = if instanceType.IsGenericType then instanceType.GetGenericTypeDefinition() else instanceType
    //    typeClasses.[typeClass].Add(actual,box <| i)

    //let arbInstances<'a> =        
    //    instance (typeof<IArbitrary<unit>>) 
    //        {new IArbitrary<unit> with
    //            override x.Arbitrary = [()]
    //            override x.Shrink _ = [] }
    //    instance (typeof<IArbitrary<bool>>) 
    //        { new IArbitrary<bool> with
    //            override x.Arbitrary = [true;false]
    //            override x.Shrink _ = [true] }
    //    instance (typeof<IArbitrary<option<'a>>>) 
    //        { new IArbitrary<option<'a>> with
    //            override x.Arbitrary = [ for i in arbitrary -> Some i]
    //            override x.Shrink a = match a with None -> [] | Some v -> [ for i in shrink v -> Some i ] }







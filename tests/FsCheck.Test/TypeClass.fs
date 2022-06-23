namespace FsCheck.Test

module TypeClass =

    open System
    open Xunit
    open FsCheck.Internals.TypeClass
    open Swensen.Unquote

    type ITypeClassUnderTest<'a> =
        abstract GetSomething : int

    [<Fact>]
    let ``should be empty on initialization``() =
        let typeClassDef = TypeClass<ITypeClassUnderTest<_>>.New()
        typedefof<ITypeClassUnderTest<_>> =! typeClassDef.Class
        test <@ typeClassDef.Instances.IsEmpty @>
        false =! typeClassDef.HasCatchAll

    [<Fact>]
    let ``should throw when intialized with non-generic type``() =
        raises<Exception> <@ TypeClass<string>.New() @>

    type PrimitiveInstance() =
        static member Int() =
            { new ITypeClassUnderTest<int> with
                override __.GetSomething = 1 }
        //to check that methods with arguments are ignored
        static member Int2(_:string) =
            { new ITypeClassUnderTest<int> with
                override __.GetSomething = 3 }

    [<Fact>]
    let ``should discover primitive types``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<PrimitiveInstance>)
        1 =! typeClass.Instances.Count
        test <@ typeClass.Instances.Contains (Primitive typeof<int>) @>
        false =! typeClass.HasCatchAll

    type ArrayInstance() =
        static member Array2() =
            { new ITypeClassUnderTest<'a[,]> with
                override __.GetSomething = 2 }

    [<Fact>]
    let ``should discover array types``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<ArrayInstance>)
        1 =! typeClass.Instances.Count
        test <@ typeClass.Instances.Contains (Array typeof<_[,]>) @>

    type CatchAllInstance() =
        static member CatchAll() =
            { new ITypeClassUnderTest<'a> with
                override __.GetSomething = 3 }

    [<Fact>]
    let ``should discover catchall``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<CatchAllInstance>)
        test <@ typeClass.HasCatchAll @>
        1 =! typeClass.Instances.Count

    [<Fact>]
    let ``should instantiate primitive type``() =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<PrimitiveInstance>)
                .InstanceFor<int,ITypeClassUnderTest<int>>()

        1 =! instance.GetSomething

    [<Fact>]
    let ``should instantiate array type``() =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<ArrayInstance>)
                .DiscoverAndMerge(true, typeof<PrimitiveInstance>) //so the int is defined too
                .InstanceFor<int[,],ITypeClassUnderTest<int[,]>>()
        2 =! instance.GetSomething

    [<Fact>]
    let ``should instantiate unknown type using catchall``() =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<CatchAllInstance>)
                .InstanceFor<string,ITypeClassUnderTest<string>>() //string not defined explicitly
        3 =! instance.GetSomething

    type InjectedInstance() =
        static member Int32() = PrimitiveInstance.Int()
        static member ListOf(element:ITypeClassUnderTest<'T>) =
            { new ITypeClassUnderTest<list<'T>> with
                member _.GetSomething = element.GetSomething + 1
            }

    [<Fact>]
    let ``should inject typeclass parameters``() =
        let discovered = 
            TypeClass<ITypeClassUnderTest<_>>
                .New(injectParameters=true)
                .Discover(true, typeof<InjectedInstance>)

        2 =! discovered.Instances.Count

        let instance = discovered.InstanceFor<list<int>,ITypeClassUnderTest<list<int>>>()
        2 =! instance.GetSomething

        let deepInstance = discovered.InstanceFor<list<list<list<int>>>,ITypeClassUnderTest<list<list<list<int>>>>>()
        4 =! deepInstance.GetSomething

    type ConfigInt = Config of int

    type InjectedConfigInstance() =
        static member String(Config c) =
            { new ITypeClassUnderTest<string> with
                member _.GetSomething = c+ 1
            }

    [<Fact>]
    let ``should inject config parameters``() =
        let discovered = 
            TypeClass<ITypeClassUnderTest<_>>
                .New(injectParameters=true)
                .Discover(true, typeof<InjectedConfigInstance>, newInjectedConfigs=[|Config 42|])

        1 =! discovered.Instances.Count

        let instance = discovered.InstanceFor<string,ITypeClassUnderTest<string>>()
        43 =! instance.GetSomething

    
    module MergeFactory = 
        [<Fact>]
        let ``should instantiate primitive type`` () =
            let instance = 
                TypeClass<ITypeClassUnderTest<_>>
                    .New()
                    .MergeFactory(fun () -> PrimitiveInstance.Int())
                    .InstanceFor<int,ITypeClassUnderTest<int>>()

            1 =! instance.GetSomething

        [<Fact>]
        let ``should inject typeclass parameters (fully-specified types only)``() =
            let discovered = 
                TypeClass<ITypeClassUnderTest<obj>>
                    .New(injectParameters=true)
                    .MergeFactory(fun () -> InjectedInstance.Int32())
                    .MergeFactory(fun (arb:ITypeClassUnderTest<int>) -> InjectedInstance.ListOf<int> arb)

            2 =! discovered.Instances.Count

            let instance = discovered.InstanceFor<list<int>,ITypeClassUnderTest<list<int>>>()
            2 =! instance.GetSomething

        [<Fact>]
        let ``should inject config parameters``() =
            let discovered = 
                TypeClass<ITypeClassUnderTest<_>>
                    .New(injectParameters=true)
                    .Discover(true, typeof<PrimitiveInstance>, newInjectedConfigs=[|Config 42|])
                    .MergeFactory(fun config -> InjectedConfigInstance.String config)

            2 =! discovered.Instances.Count

            let instance = discovered.InstanceFor<string,ITypeClassUnderTest<string>>()
            43 =! instance.GetSomething

        [<Fact>]
        let ``should handle factories that require context`` () = 
            let context = (System.Random()).Next()
            let instance = 
                TypeClass<ITypeClassUnderTest<_>>
                    .New()
                    .MergeFactory(fun () -> { 
                        new ITypeClassUnderTest<int> with
                            override __.GetSomething = context })
                    .InstanceFor<int,ITypeClassUnderTest<int>>()

            context =! instance.GetSomething

        [<Fact>]
        let ``should override existing factories of same type`` () = 
            let original = PrimitiveInstance.Int()
            let expected = { 
                        new ITypeClassUnderTest<int> with
                            override __.GetSomething = original.GetSomething + 2 }
            let instance = 
                TypeClass<ITypeClassUnderTest<_>>
                    .New()
                    .MergeFactory(fun () -> original)
                    .MergeFactory(fun () -> expected)
                    .InstanceFor<int,ITypeClassUnderTest<int>>()

            expected.GetSomething =! instance.GetSomething

        let raises<'T when 'T :> Exception> (f:unit->unit) =
            // Unquote doesn't support the expression I needed to evaluate
            let didThrow = 
                try 
                    f()
                    false
                with 
                | :? 'T -> true
                | e -> failwithf "Expected %s, got %A" typeof<'T>.Name e

            if not didThrow then failwithf "Expected exception %s, but no exception was thrown" typeof<'T>.Name



        [<Fact>]
        let ``should error if factory returns incompatible type`` () = 
            raises<ArgumentException> <| fun () ->
                TypeClass<int list>
                    .New()
                    .MergeFactory(fun () -> 5)
                    |> ignore

        [<Fact>]
        let ``should error if factory requires unavailable parameters`` () = 
            raises<ArgumentException> <| fun () ->
                TypeClass<int list>
                    .New()
                    .MergeFactory(fun (i:int) -> [i])
                    |> ignore

            
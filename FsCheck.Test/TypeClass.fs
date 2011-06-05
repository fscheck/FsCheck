namespace FsCheck.Test

module TypeClass =

    open System
    open Xunit
    open FsCheck
    open FsCheck.TypeClass

    type ITypeClassUnderTest<'a> =
        abstract GetSomething : int

    [<Fact>]
    let ``should be empty on initialization``() =
        let typeClassDef = TypeClass<ITypeClassUnderTest<_>>.New()
        Assert.Equal(typedefof<ITypeClassUnderTest<_>>,typeClassDef.Class)
        Assert.Empty typeClassDef.ArrayInstances 
        Assert.Empty typeClassDef.Instances
        Assert.False typeClassDef.HasCatchAll

    [<Fact>]
    let ``should throw when intialized with non-generic type``() =
        Assert.Throws<Exception>(fun () -> TypeClass<string>.New() |> ignore) 
        |> ignore

    type PrimitiveInstance() =
        static member Int() =
            { new ITypeClassUnderTest<int> with
                override x.GetSomething = 1 }

    [<Fact>]
    let ``should discover primitive types``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<PrimitiveInstance>)
        Assert.Equal(1, typeClass.Instances.Count)
        Assert.Contains((typeof<int>.FullName, Set.empty), typeClass.Instances)
        Assert.Empty typeClass.ArrayInstances 
        Assert.False typeClass.HasCatchAll

    type ArrayInstance() =
        static member Array2() =
            { new ITypeClassUnderTest<'a[,]> with
                override x.GetSomething = 2 }

    [<Fact>]
    let ``should discover array types``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<ArrayInstance>)
        Assert.Equal(1, typeClass.ArrayInstances.Count)
        Assert.Contains((2,Set.empty), typeClass.ArrayInstances)
        Assert.Empty typeClass.Instances 
        Assert.False typeClass.HasCatchAll

    type CatchAllInstance() =
        static member CatchAll() =
            { new ITypeClassUnderTest<'a> with
                override x.GetSomething = 3 }

    [<Fact>]
    let ``should discover catchall``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<CatchAllInstance>)
        Assert.True typeClass.HasCatchAll
        Assert.Empty(typeClass.Instances)
        Assert.Empty typeClass.ArrayInstances 


    // Now test instances parametrized on attributes

    [<AttributeUsage(AttributeTargets.Parameter)>]
    type GenParameterAttribute(first:int,second:string) =
        inherit Attribute()
        member x.First = first
        member x.Second = second

    [<AttributeUsage(AttributeTargets.Parameter)>]
    type AnotherGenParameterAttribute(first:double) =
        inherit Attribute()
        member x.First = first

    type PrimitiveInstanceWithAttribute() =
        static member Int(attribute:GenParameterAttribute) =
            { new ITypeClassUnderTest<int> with
                override x.GetSomething = attribute.First }
        static member Int(attribute:GenParameterAttribute, anotherAttribute:AnotherGenParameterAttribute) =
            { new ITypeClassUnderTest<int> with
                override x.GetSomething = attribute.First + 10 * int anotherAttribute.First }

    [<Fact>]
    let ``should discover primitive types parametrized on attribute``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<PrimitiveInstanceWithAttribute>)
        let expectedInstances =
            [ (typeof<int>.FullName, typeof<GenParameterAttribute>.FullName  |> Set.singleton)
              (typeof<int>.FullName, [ typeof<GenParameterAttribute>.FullName; typeof<AnotherGenParameterAttribute>.FullName]  |> Set.ofList)
            ] |> Set.ofList

        Assert.Equal(2, typeClass.Instances.Count)
        Assert.Equal(expectedInstances, typeClass.Instances)
        Assert.Empty typeClass.ArrayInstances 
        Assert.False typeClass.HasCatchAll

    type ArrayInstanceWithAttributes() =
        static member Array(attribute:GenParameterAttribute, attribute2:AnotherGenParameterAttribute) =
            { new ITypeClassUnderTest<'a[]> with
                override x.GetSomething = attribute.First + 10 * int attribute2.First }
        static member Array(attribute:GenParameterAttribute) =
            { new ITypeClassUnderTest<'a[]> with
                override x.GetSomething = attribute.First  }

    [<Fact>]
    let ``should discover array types parametrized on attribute``() = 
        let typeClass = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<ArrayInstanceWithAttributes>)

        let expectedInstances =
            [ (1, typeof<GenParameterAttribute>.FullName  |> Set.singleton)
              (1, [ typeof<GenParameterAttribute>.FullName; typeof<AnotherGenParameterAttribute>.FullName]  |> Set.ofList)
            ] |> Set.ofList
        Assert.Equal(expectedInstances, typeClass.ArrayInstances)
        Assert.Empty typeClass.Instances 
        Assert.False typeClass.HasCatchAll

    [<Fact>]
    let ``should instantiate primitive type``() =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<PrimitiveInstance>)
                .InstanceFor<int,ITypeClassUnderTest<int>>()

        Assert.Equal(1, instance.GetSomething)

    [<Fact>]
    let ``should instantiate array type``() =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<ArrayInstance>)
                .DiscoverAndMerge(true, typeof<PrimitiveInstance>) //so the int is defined too
                .InstanceFor<int[,],ITypeClassUnderTest<int[,]>>()
        Assert.Equal(2, instance.GetSomething)

    [<Fact>]
    let ``should instantiate unknown type using catchall``() =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<CatchAllInstance>)
                .InstanceFor<string,ITypeClassUnderTest<string>>() //string not defined explicitly
        Assert.Equal(3, instance.GetSomething)
                 
    [<Property(MaxTest=10)>]
    let ``should instantiate primitive type with attributes``(parameter:int) =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<PrimitiveInstanceWithAttribute>)
                .InstanceFor<int,ITypeClassUnderTest<int>>([new GenParameterAttribute(parameter,"")])
        
        Assert.Equal(parameter, instance.GetSomething)

    [<Property(MaxTest=10)>]
    let ``should instantiate array type with attributes``(parameter:int, parameter2:int) =
        let instance = 
            TypeClass<ITypeClassUnderTest<_>>
                .New()
                .Discover(true, typeof<ArrayInstanceWithAttributes>)
                .DiscoverAndMerge(true, typeof<PrimitiveInstance>) //so the int is defined too
                .InstanceFor<int[],ITypeClassUnderTest<int[]>>([new GenParameterAttribute(parameter,""); new AnotherGenParameterAttribute(float parameter2)])

        Assert.Equal(parameter + 10 * parameter2, instance.GetSomething)






    
        
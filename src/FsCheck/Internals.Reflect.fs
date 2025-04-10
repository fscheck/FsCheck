namespace FsCheck.Internals

module internal Reflect =

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Reflection
    open System.Reflection
    open System.Linq.Expressions

    let isRecordType (ty : Type) = FSharpType.IsRecord(ty, true)

    let isUnionType ty = FSharpType.IsUnion(ty, true)

    let isTupleType ty = FSharpType.IsTuple ty

    let getPublicCtors (ty: Type) = ty.GetTypeInfo().DeclaredConstructors |> Seq.filter (fun c -> c.IsPublic)

    let getPropertyMethod (prop:PropertyInfo) =
        // prop.GetMethod can be null if property only has a setter
        if isNull prop.GetMethod then prop.SetMethod else prop.GetMethod
        
    let getProperties (ty: Type) = ty.GetRuntimeProperties() |> Seq.filter (fun p -> let m = getPropertyMethod p in  not m.IsStatic && m.IsPublic)

    let isInitOnlyProperty(property: PropertyInfo) =
#if NETSTANDARD1_0
        false
#else
        property.CanWrite
        && (property.SetMethod.ReturnParameter.GetRequiredCustomModifiers()
            |> Array.exists (fun t -> t.FullName = "System.Runtime.CompilerServices.IsExternalInit"))
#endif

    /// An immutable "record-like" type:
    /// - must have a single public constructor
    /// - must not have any property setters (not even C# init)
    /// - must have all init-only fields (readonly in C# - these fields can only be set in the ctor)
    /// These can be generated but not shrunk, because there is no reliable way to build a reader,
    /// i.e. get the ctor parameter values back out from an existing value, because there's no 
    /// guarantee that the properties map back to the constructor arguments.
    let isImmutableRecordLikeType (ty: Type) =
        let typeinfo = ty.GetTypeInfo()
        not typeinfo.IsAbstract
        && not typeinfo.ContainsGenericParameters
        && Seq.length (getPublicCtors ty) = 1
        && not (ty.GetRuntimeProperties() |> Seq.filter (fun m -> not m.GetMethod.IsStatic && m.GetMethod.IsPublic) |> Seq.exists (fun p -> p.CanWrite))
        && ty.GetRuntimeFields() |> Seq.filter (fun m -> not m.IsStatic && m.IsPublic) |> Seq.forall (fun f -> f.IsInitOnly)


    /// A "C# record" type:
    /// - must have a single public constructor
    /// - must have a correspondingly named init-only property for every ctor argument
    /// - may have additional settable properties (set or init)
    /// This should cover types declared using C#'s record syntax, but also others.
    /// As opposed to "immutable record-like" types, these can be shrunk automatically.
    let isCSharpRecordType (ty: Type) =
        let typeinfo = ty.GetTypeInfo()
        let props = getProperties ty |> Seq.toArray

        let recordPropNames =
            if ty.IsValueType then
                // Struct records are mutable unless explicitly marked `readonly`:
                //     public record struct MutableStructRecord(int A);
                props |> Array.filter _.CanWrite |> Array.map _.Name
            else
                // Immutable records:
                //     public record ImmutableClassRecord(int A);
                //     public record class ImmutableClassRecord(int A);
                //     public readonly record struct ImmutableStructRecord(int A);
                props |> Array.filter isInitOnlyProperty |> Array.map _.Name

        let hasRecordCtor =
            // either no parameters, or for each parameter there is a corresponding
            // init-only property which we can set. THis is what C# generates with its
            // record syntax.
            match getPublicCtors ty |> Array.ofSeq with
            | [| ctor |] ->
                ctor.GetParameters() 
                |> Seq.forall (fun param -> recordPropNames |> Array.contains param.Name)
            // Mutable struct record with no positional properties.
            | [||] -> ty.IsValueType
            | _ -> false
        let hasWritableProperties =
            props |> Array.exists (fun p -> p.CanWrite)
        not typeinfo.IsAbstract
        && not typeinfo.ContainsGenericParameters
        && hasRecordCtor
        && hasWritableProperties

    /// A collection type in the System.Collections.Immutable namespace.
    let isImmutableCollectionType (ty: Type) =
        ty.FullName.StartsWith("System.Collections.Immutable")
        && Array.contains ty.Name [| 
                "ImmutableArray`1"; "ImmutableHashSet`1"; "ImmutableList`1" 
                "ImmutableQueue`1"; "ImmutableSortedSet`1"; "ImmutableStack`1"
                "ImmutableDictionary`2"; "ImmutableSortedDictionary`2"
                |]


    /// Get information on the fields of an F# record type
    let getRecordFieldTypes (recordType: System.Type) = 
        if isRecordType recordType then 
            FSharpType.GetRecordFields(recordType, true)
            |> Array.map (fun pi -> pi.PropertyType)
        else 
            failwithf "The input type must be an F# record type.  Got %A" recordType

    /// Get constructor for F# record type
    let getRecordConstructor recordType = 
        FSharpValue.PreComputeRecordConstructor(recordType, true)

    /// Get reader for F# record type
    let getRecordReader recordType = 
        FSharpValue.PreComputeRecordReader(recordType, true)

    let getImmutableRecordLikeTypeFields (recordType: Type) =
        if isImmutableRecordLikeType recordType then
            let ctor = getPublicCtors recordType |> Seq.head
            ctor.GetParameters() |> Seq.map (fun p -> p.ParameterType)
        else
            failwithf "The input type must be an immutable class with a single constructor. Got %A" recordType
        
    let getImmutableRecordLikeTypeConstructor (t:Type) =
        let ctor  = getPublicCtors t |> Seq.head
        let ctorps= ctor.GetParameters ()
        let par   = Expression.Parameter (typeof<obj[]>, "args")
        let pars  = ctorps |> Array.mapi (fun i p ->  Expression.Convert (
                                                          Expression.ArrayIndex (par, Expression.Constant i),
                                                          p.ParameterType)
                                                      :> Expression)
        let body  = Expression.New (ctor, pars)
        let bodyAsObject = Expression.Convert (body, typeof<Object>)
        let l     = Expression.Lambda<Func<obj[], obj>> (bodyAsObject, par)
        let f     = l.Compile ()
        f.Invoke

    let getCSharpRecordFields (recordType: Type) =
        if isCSharpRecordType recordType then
            getProperties recordType
            |> Seq.filter (fun p -> p.CanWrite)
            |> Seq.map (fun p -> p.PropertyType)
        else
            failwithf "The input type must be a C# record-like type. Got %A" recordType

    let getCSharpRecordConstructor (t:Type) =
        let props = getProperties t |> Seq.filter (fun p -> p.CanWrite) |> Seq.toArray
  
        let par = Expression.Parameter (typeof<obj[]>, "args")

        let maybeCtor = getPublicCtors t |> Seq.tryHead

        let pars =
            maybeCtor
            |> Option.map (_.GetParameters() >> Array.mapi (fun i p ->
                Expression.Convert
                    (Expression.ArrayIndex (par, Expression.Constant i), p.ParameterType) : Expression))
            |> Option.defaultValue [||]

        let values = 
            props 
            |> Seq.mapi (fun i p ->  
                let idx = Expression.ArrayIndex (par, Expression.Constant i)
                Expression.Convert (idx, p.PropertyType) :> Expression)
        let bindings =
            props
            |> Seq.zip values
            |> Seq.map (fun (v, p) -> Expression.Bind(p, v) :> MemberBinding)

        let newExpr =
            maybeCtor
            |> Option.map (fun ctor -> Expression.New (ctor, pars))
            |> Option.defaultWith (fun () -> Expression.New t)

        let body =
            if t.IsValueType then
                Expression.Convert (Expression.MemberInit (newExpr, bindings), typeof<obj>) : Expression
            else
                Expression.MemberInit (newExpr, bindings)

        let l = Expression.Lambda<Func<obj[], obj>> (body, par)
        let f = l.Compile ()
        f.Invoke

    let getCSharpRecordReader (recordType: Type) =
        if isCSharpRecordType recordType then
            let properties = getProperties recordType
                             |> Seq.filter (fun p -> p.CanWrite)
                             |> Seq.map (fun p -> p.GetValue)
                             |> Seq.toArray
            let lookup o = Array.map (fun f -> f o) properties
            lookup
        else
            failwithf "The input type must be a C# record-like class. Got %A" recordType

    /// Returns the case name, type, and functions that will construct a constructor and a reader of a union type respectively
    let getUnionCases unionType : (string * (int * System.Type list * (obj[] -> obj) * (obj -> obj[]))) list = 
        [ for case in FSharpType.GetUnionCases(unionType, true) ->
            let types =    [ for fld in case.GetFields() -> fld.PropertyType ]
            let ctorFn =   FSharpValue.PreComputeUnionConstructor(case, true)
            let readerFn = FSharpValue.PreComputeUnionReader(case, true)
                
            case.Name, (case.Tag, types, ctorFn, readerFn)]

    /// Get reader for union case name (aka tag)
    let getUnionTagReader unionType = 
        FSharpValue.PreComputeUnionTagReader(unionType, true)
                
    // resolve fails if the generic type is only determined by the return type 
    //(e.g., Array.zero_create) but that is easily fixed by additionally passing in the return type...
    let rec private resolve (acc:Dictionary<_,_>) (a:Type, f:Type) =
        if f.IsGenericParameter then
            if not (acc.ContainsKey(f)) then acc.Add(f,a)
        else 
            if a.HasElementType then resolve acc (a.GetElementType(), f.GetElementType())
            Array.zip (a.GenericTypeArguments) (f.GenericTypeArguments) 
            |> Array.iter (resolve acc)

    let invokeMethod (m:MethodInfo) target args =
        let m = if m.ContainsGenericParameters then
                    let typeMap = new Dictionary<_,_>()
                    Array.zip args (m.GetParameters()) |> 
                    Array.iter (fun (a,f) -> resolve typeMap (a.GetType(),f.ParameterType))
                    let actuals = 
                        m.GetGenericArguments() |> 
                        Array.map (fun formal -> typeMap.[formal])
                    m.MakeGenericMethod(actuals)
                else 
                    m
        match target with 
        | None -> m.Invoke(null, args)
        | Some t -> m.Invoke(t, args)

    /// Returns a function that creates the given System.Collections.Immutable type,
    /// with a single generic type parameter, from an array.
    let getImmutableCollection1Constructor (t:Type) (elementType: Type) =
        let staticTypeName = t.GetGenericTypeDefinition().AssemblyQualifiedName.Replace("`1", "")
        let staticType = Type.GetType(staticTypeName, throwOnError=true)
        let createMethod = 
            staticType.GetRuntimeMethods()
            |> Seq.find(fun (mi:MethodInfo) -> 
                            let parameters = mi.GetParameters()
                            mi.IsPublic && mi.IsStatic && mi.Name = "Create" 
                            && parameters.Length = 1 && parameters.[0].ParameterType.IsArray)
        let genericCreateMethod = createMethod.MakeGenericMethod(elementType)
        fun arr -> genericCreateMethod.Invoke(null, [| arr |])

    /// Returns a function that reads the given System.Collections.Immutable type,
    /// with a single generic type parameter, as an array.
    let getImmutableCollection1Reader (elementType: Type) =
        let toArrayMethod =
            typeof<Linq.Enumerable>.GetRuntimeMethods()
            |> Seq.find (fun mi -> mi.IsPublic && mi.IsStatic && mi.Name = "ToArray" && mi.GetParameters().Length = 1)
        let genericToArrayMethod = toArrayMethod.MakeGenericMethod(elementType)
        fun o -> genericToArrayMethod.Invoke(null, [| o |])


    /// Returns a function that creates the given System.Collections.Immutable type
    /// with two generic type parameters, from an IEnumerable<KeyValuePair<_,_,>>.
    let getImmutableCollection2Constructor (t: Type) (genericArguments: Type[]) =
        let staticTypeName = t.GetGenericTypeDefinition().AssemblyQualifiedName.Replace("`2", "")
        let staticType = Type.GetType(staticTypeName, throwOnError=true)
        let createRangeMethod = 
            staticType.GetRuntimeMethods()
            |> Seq.find(fun (mi:MethodInfo) -> 
                            let parameters = mi.GetParameters()
                            mi.IsPublic && mi.IsStatic && mi.Name = "CreateRange"
                            && parameters.Length = 1 && parameters.[0].ParameterType.GetGenericTypeDefinition() = typedefof<Collections.Generic.IEnumerable<_>>)
        let genericCreateRangeMethod = createRangeMethod.MakeGenericMethod(genericArguments)
        fun dict -> genericCreateRangeMethod.Invoke(null, [| dict |])
namespace FsCheck

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

    let isCSharpRecordType (ty: Type) = 
        let typeinfo = ty.GetTypeInfo()
        typeinfo.IsClass && not typeinfo.IsAbstract
        && not typeinfo.ContainsGenericParameters
        && Seq.length (getPublicCtors ty) = 1
        && not (ty.GetRuntimeProperties() |> Seq.filter (fun m -> not m.GetMethod.IsStatic && m.GetMethod.IsPublic) |> Seq.exists (fun p -> p.CanWrite))
        && ty.GetRuntimeFields() |> Seq.filter (fun m -> not m.IsStatic && m.IsPublic) |> Seq.forall (fun f -> f.IsInitOnly)

    /// Get information on the fields of a record type
    let getRecordFieldTypes (recordType: System.Type) = 
        if isRecordType recordType then 
            FSharpType.GetRecordFields(recordType, true)
            |> Array.map (fun pi -> pi.PropertyType)
        else 
            failwithf "The input type must be a record type.  Got %A" recordType

    /// Get constructor for record type
    let getRecordConstructor recordType = 
        FSharpValue.PreComputeRecordConstructor(recordType, true)

    /// Get reader for record type
    let getRecordReader recordType = 
        FSharpValue.PreComputeRecordReader(recordType, true)

    let getCSharpRecordFields (recordType: Type) =
        if isCSharpRecordType recordType then
            let ctor = getPublicCtors recordType |> Seq.head
            ctor.GetParameters() |> Seq.map (fun p -> p.ParameterType)
        else
            failwithf "The input type must be an immutable class with a single constructor. Got %A" recordType
        
    let getCSharpRecordConstructor (t:Type) =
        let ctor  = getPublicCtors t |> Seq.head
        let ctorps= ctor.GetParameters ()
        let par   = Expression.Parameter (typeof<obj[]>, "args")
        let pars  = ctorps |> Array.mapi (fun i p ->  Expression.Convert (
                                                          Expression.ArrayIndex (par, Expression.Constant i),
                                                          p.ParameterType)
                                                      :> Expression)
        let body  = Expression.New (ctor, pars)
        let l     = Expression.Lambda<Func<obj[], obj>> (body, par)
        let f     = l.Compile ()
        f.Invoke

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
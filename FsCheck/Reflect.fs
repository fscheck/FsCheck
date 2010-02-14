(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2010 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

module internal Reflect =

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Reflection
    open System.Reflection

    let private recordFieldBindingFlags = 
        BindingFlags.GetProperty ||| BindingFlags.Instance 
        ||| BindingFlags.NonPublic ||| BindingFlags.Public

    let isRecordType (ty : Type) = FSharpType.IsRecord(ty, recordFieldBindingFlags)
    let isUnionType ty = FSharpType.IsUnion ty
    let isTupleType ty = FSharpType.IsTuple ty

    /// Get information on the fields of a record type
    let getRecordFields (recordType: System.Type) = 
        if isRecordType recordType then 
            FSharpType.GetRecordFields(recordType, recordFieldBindingFlags)
        else 
            failwithf "The input type must be a record type.  Got %A" recordType

    /// Get constructor for record type
    let getRecordConstructor recordType = 
        FSharpValue.PreComputeRecordConstructor(recordType, recordFieldBindingFlags)              

    /// Get reader for record type
    let getRecordReader recordType = 
        FSharpValue.PreComputeRecordReader(recordType, recordFieldBindingFlags)
        

    /// Returns the case name, type, and functions that will construct a constructor and a reader of a union type respectively
    let getUnionCases unionType : (string * (int * System.Type list * (obj[] -> obj) * (obj -> obj[]))) list = 
        [ for case in FSharpType.GetUnionCases(unionType, recordFieldBindingFlags) -> 
            let types =    [ for fld in case.GetFields() -> fld.PropertyType ]              
            let ctorFn =   FSharpValue.PreComputeUnionConstructor(case, recordFieldBindingFlags)                           
            let readerFn = FSharpValue.PreComputeUnionReader(case, recordFieldBindingFlags)
                
            case.Name, (case.Tag, types, ctorFn, readerFn)]

    /// Get reader for union case name (aka tag)
    let getUnionTagReader unionType = 
        FSharpValue.PreComputeUnionTagReader(unionType, recordFieldBindingFlags)

    ///Equality for generic types    
    let genericTypeEq (lhs: System.Type) (rhs: System.Type) : bool =
            lhs.IsGenericType && rhs.IsGenericType &&
                (lhs.GetGenericTypeDefinition() = rhs.GetGenericTypeDefinition())
                
    // resolve fails if the generic type is only determined by the return type 
    //(e.g., Array.zero_create) but that is easily fixed by additionally passing in the return type...
    let rec private resolve (acc:Dictionary<_,_>) (a:Type, f:Type) =
        if f.IsGenericParameter then
            if not (acc.ContainsKey(f)) then acc.Add(f,a)
        else 
            if a.HasElementType then resolve acc (a.GetElementType(), f.GetElementType())
            Array.zip (a.GetGenericArguments()) (f.GetGenericArguments()) |>
            Array.iter (resolve acc)

    let internal invokeMethod (m:MethodInfo) target args =
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
        match target with None -> m.Invoke(null, args) | Some t -> m.Invoke(t,args)

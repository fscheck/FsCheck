(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2009 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

module internal Reflect

open System
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
    FSharpValue.PrecomputeRecordConstructor(recordType, recordFieldBindingFlags)              

/// Get reader for record type
let getRecordReader recordType = 
    FSharpValue.PrecomputeRecordReader(recordType, recordFieldBindingFlags)
    

/// Returns the case name, type, and functions that will construct a constructor and a reader of a union type respectively
let getUnionCases unionType : (string * (int * System.Type list * (obj[] -> obj) * (obj -> obj[]))) list = 
    [ for case in FSharpType.GetUnionCases(unionType, recordFieldBindingFlags) -> 
        let types =    [ for fld in case.GetFields() -> fld.PropertyType ]              
        let ctorFn =   FSharpValue.PrecomputeUnionConstructor(case, recordFieldBindingFlags)                           
        let readerFn = FSharpValue.PrecomputeUnionReader(case, recordFieldBindingFlags)
            
        case.Name, (case.Tag, types, ctorFn, readerFn)]

/// Get reader for union case name (aka tag)
let getUnionTagReader unionType = 
    FSharpValue.PrecomputeUnionTagReader(unionType, recordFieldBindingFlags)

///Equality for generic types    
let genericTypeEq (lhs: System.Type) (rhs: System.Type) : bool =
        lhs.IsGenericType && rhs.IsGenericType &&
            (lhs.GetGenericTypeDefinition() = rhs.GetGenericTypeDefinition())

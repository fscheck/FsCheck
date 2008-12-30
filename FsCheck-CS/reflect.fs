#light

namespace FSCheck

open System

open Microsoft.FSharp.Reflection
open System.Reflection

module Reflect = 

    let gmagFailWithf = failwithf
    let gmagFailWith = failwith

    type Either<'a,'b> = 
        | Left of 'a
        | Right of 'b


    let private recordFieldBindingFlags = BindingFlags.GetProperty ||| BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public

    let isRecordType (ty : System.Type) = FSharpType.IsRecord(ty, recordFieldBindingFlags)
    let isUnionType ty = FSharpType.IsUnion ty
    let isTupleType ty = FSharpType.IsTuple ty

    /// Get information on the fields of a record type
    let getRecordFields (recordType: System.Type) = 
        if isRecordType recordType then 
            FSharpType.GetRecordFields(recordType, recordFieldBindingFlags)
        else 
            gmagFailWithf "The input type must be a record type.  Got %A" recordType

    /// Get constructor for record type
    let getRecordConstructor recordType = 
        FSharpValue.PrecomputeRecordConstructor(recordType, recordFieldBindingFlags)              

    /// Get reader for record type
    let getRecordReader recordType = 
        FSharpValue.PrecomputeRecordReader(recordType, recordFieldBindingFlags)
        
    /// Get information on union type cases, supporting on only unary union cases
    /// Returns the case name, type, and functions that will construct a constructor and a reader respectively
    let getUnionCases unionType : (string * (int * System.Type list * (obj[] -> obj) * (obj -> obj[]))) list = 
        [ for case in FSharpType.GetUnionCases(unionType, recordFieldBindingFlags) -> 
            let types =    [ for fld in case.GetFields() -> fld.PropertyType ]              
            let ctorFn =   FSharpValue.PrecomputeUnionConstructor(case, recordFieldBindingFlags)                           
            let readerFn = FSharpValue.PrecomputeUnionReader(case, recordFieldBindingFlags)
                
            case.Name, (case.Tag, types, ctorFn, readerFn)]

    /// Get reader for union case name (aka tag)
    let getUnionTagReader unionType = 
        FSharpValue.PrecomputeUnionTagReader(unionType, recordFieldBindingFlags)

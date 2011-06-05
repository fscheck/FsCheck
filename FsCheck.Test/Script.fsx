open System
open System.Reflection
open Microsoft.FSharp.Reflection

let func = (fun ([<param:GenParameterAttribute>] test:int) -> 1)

let funcType = func.GetType()

let invoke = funcType.GetMethod("Invoke")

invoke.GetParameters().[0].GetCustomAttributes(true)

FSharpType.GetFunctionElements funcType |> fst

[<AttributeUsageAttribute(AttributeTargets.Parameter)>]
type ParameterAttribute(i:int) =
    inherit Attribute()



//this works - the attribute is applied to the static member
let funcy ([<param:ParameterAttribute(1)>] test:int) = 1

//none of the below does anything
let func2 = (fun ([<param:GenParameterAttribute(1,"1")>] test:int) -> 1)
let funcNonsense = (fun ([<ICanTypeWhateverIWantHereAndItStillCompiles>] test:int) -> 1)



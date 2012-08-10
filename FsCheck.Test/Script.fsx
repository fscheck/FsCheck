open System
open System.Reflection
open Microsoft.FSharp.Reflection

let func = (fun ([<param:GenParameterAttribute>] test:int) -> 1)

let funcType = func.GetType()

let invoke = funcType.GetMethod("Invoke")

invoke.GetParameters().[0].GetCustomAttributes(true)

FSharpType.GetFunctionElements funcType |> fst

[<AttributeUsageAttribute(AttributeTargets.Parameter ||| AttributeTargets.GenericParameter)>]
type ParameterAttribute(i:int) =
    inherit Attribute()


//this works - the attribute is applied to the static member
let funcy ([<param:ParameterAttribute(1)>] test:list<int>) = 1

//none of the below does anything
let func2 = (fun ([<param:GenParameterAttribute(1,"1")>] test:int) -> 1)
let funcNonsense = (fun ([<ICanTypeWhateverIWantHereAndItStillCompiles>] test:int) -> 1)


//type Attributed<'a>(first:obj, ?second:obj, ?third:obj, ?fourth:obj, ?fifth:obj, ?sixth:obj) = class end

//type WithParams<[<Parameter<_>(<@ 5 @>)>]'a (* property *)> = Attributed of 'a

//let myTest : WithParams<_> = Attributed (fun a -> 1+a) ///,[| ParameterAttribute(5) |])




let (|Apply|) f input = f input

let addFunc = fun a -> a+5

let add5 (Apply addFunc res) = res

let add5' (Apply ((+) 5) res) = res

//let add5'' (Apply (fun a -> a + 5) res) = res 




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

//applicative
#r "bin\Debug\FsCheck.dll"
open FsCheck

//how to generate float between 0 and 5
let f = 
    Arb.generate<NormalFloat>
    |> Gen.map float
    |> Gen.resize 5
    |> Gen.map abs
    |> Gen.suchThat (fun f -> f < 5.)
    |> Gen.sample 100 10

let f = fun a b c -> a + b - c
let ga,gb,gc = Gen.constant 3, Gen.constant 3, Gen.constant 3
let g = f <!> ga <*> gb <*> gc

#light

namespace FsCheck

//module public Fluent

open System
open System.Linq

//Label
//And
//Or
//Within
//Throws

type IPropertyBuilder =
    abstract Build : unit -> Property

type Any = 
    static member OfType<'a>() = arbitrary<'a>

type Spec() =
    member x.For<'a>(generator:'a Gen, assertion:Func<'a,bool>) =
        SpecBuilder<'a>(generator, assertion, [], [], [])
    member x.For<'a,'b>(generator1:'a Gen,generator2:'b Gen, assertion:Func<'a,'b,bool>) =
        SpecBuilder<'a,'b>(generator1, generator2, assertion,[],[],[])

and SpecBuilder<'a>(generator0:'a Gen, assertion0:Func<'a,bool>, conditions:Func<'a,bool> list, collects:Func<'a,string> list, classifies:(Func<'a,bool> * string) list) =
    interface IPropertyBuilder with
        member x.Build() =
            let conditions' a = conditions |> List.fold_left (fun s f -> s && f.Invoke(a)) true
            let collects' a prop = collects |> List.fold_left (fun prop f -> prop |> collect (f.Invoke(a))) prop
            let classifies' a prop = classifies |> List.fold_left (fun prop (f,name) -> prop |> classify (f.Invoke(a)) name) prop  
            forAll generator0 (fun a -> (conditions' a) ==> (assertion0.Invoke(a)) |> collects' a |> classifies' a)
    member x.When( condition:Func<'a,bool> ) = 
        SpecBuilder<'a>(generator0, assertion0, condition::conditions, collects, classifies)
    member x.Collect(collectedValue:Func<'a,string>)=
        SpecBuilder<'a>(generator0,assertion0,conditions,collectedValue::collects,classifies)
    member x.Classify(filter:Func<'a,bool>,name:string) =
        SpecBuilder<'a>(generator0,assertion0,conditions,collects,(filter,name)::classifies)
    member x.AndFor<'b>(generator:'b Gen, assertion:Func<'b,bool>) =
        SpecBuilder<'a,'b>  (generator0
                            ,generator
                            ,fun a b -> assertion0.Invoke(a) && assertion.Invoke(b)
                            ,conditions |> List.map (fun f -> Func<'a,'b,bool>(fun a b -> f.Invoke(a)))
                            ,collects |> List.map (fun f -> Func<'a,'b,string>(fun a b -> f.Invoke(a)))
                            ,classifies |> List.map (fun (f,name) -> (Func<'a,'b,bool>(fun a b -> f.Invoke(a)),name))
                            )
       
and SpecBuilder<'a,'b>(generator0:'a Gen,generator1:'b Gen, 
                        assertion:Func<'a,'b,bool>,
                        conditions:Func<'a,'b,bool> list, 
                        collects:Func<'a,'b,string> list, 
                        classifies:(Func<'a,'b,bool> * string) list) = 
    member x.When() = ()
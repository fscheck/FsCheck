#light

namespace FsCheck

//module public Fluent //don't add a module - otherwise every class needs to be qualified
//alternatively, give the module the name Spec and use let bound functions for For<...>

open System
open System.Linq
open System.ComponentModel
open System.Collections.Generic

//TODO:
//Label
//And
//Or
//Within
//Throws

//shrinking??

//type IPropertyBuilder =
//    abstract QuickCheck : unit -> unit
//    abstract VerboseCheck : unit -> unit

type WeightAndValue<'a>(weight:int,value:'a) =
    member x.Weight = weight
    member x.Value = value
    

type Any = 
    static member OfType<'a>() = arbitrary<'a>
    static member Value (value) = constant value
    static member ValueIn (values : seq<_>) = values |> Seq.to_list |> elements
    static member IntBetween (l,h) = choose (l,h)
    static member GeneratorIn (generators : seq<Gen<_>>) = generators |> Seq.to_list |> oneof
    static member WeighedGeneratorIn ( weighedValues : seq<WeightAndValue<Gen<'a>>> ) =
        weighedValues |> Seq.map (fun wv -> (wv.Weight, wv.Value)) |> Seq.to_list |> frequency
    static member ListOf<'a> (generator) = listOf generator |> fmapGen (fun list -> new List<'a>(list))
    static member NonEmptyListOf<'a> (generator) = nonEmptyListOf generator |> fmapGen (fun list -> new List<'a>(list))
    static member ListOfN<'a> (count, generator) = vectorOf count generator |> fmapGen (fun list -> new List<'a>(list))
    static member SequenceOf<'a> (generators:seq<Gen<_>>) = generators |> Seq.to_list |> sequence |> fmapGen (fun list -> new List<'a>(list))
 

[<AbstractClass>]
type UnbrowsableObject() =
    inherit obj()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override x.Equals(other) = base.Equals(other)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override x.GetHashCode() = base.GetHashCode()
//    [<EditorBrowsable(EditorBrowsableState.Never)>]
//    override x.GetType() = base.GetType()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override x.ToString() = base.ToString()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    abstract Build : unit -> Property
    member x.QuickCheck() = quickCheck <| x.Build()
    member x.VerboseCheck() = verboseCheck <| x.Build()

and SpecBuilder<'a>( generator0:'a Gen
                   , assertion0:Func<'a,bool>
                   , conditions:Func<'a,bool> list
                   , collects:Func<'a,string> list
                   , classifies:(Func<'a,bool> * string) list) =
    inherit UnbrowsableObject()
    override x.Build() =
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
                        assertion0:Func<'a,'b,bool>,
                        conditions:Func<'a,'b,bool> list, 
                        collects:Func<'a,'b,string> list, 
                        classifies:(Func<'a,'b,bool> * string) list) = 
    inherit UnbrowsableObject()
    override x.Build() =
            let conditions' a b = conditions |> List.fold_left (fun s f -> s && f.Invoke(a,b)) true
            let collects' a b prop = collects |> List.fold_left (fun prop f -> prop |> collect (f.Invoke(a,b))) prop
            let classifies' a b prop = classifies |> List.fold_left (fun prop (f,name) -> prop |> classify (f.Invoke(a,b)) name) prop  
            forAll generator0 (fun a -> forAll generator1 (fun b -> (conditions' a b) ==> (assertion0.Invoke(a,b)) |> collects' a b |> classifies' a b))
    member x.When( condition:Func<'a,'b,bool> ) = 
        SpecBuilder<'a,'b>(generator0, generator1, assertion0, condition::conditions, collects, classifies)
    member x.Collect(collectedValue:Func<'a,'b,string>)=
        SpecBuilder<'a,'b>(generator0, generator1, assertion0,conditions,collectedValue::collects,classifies)
    member x.Classify(filter:Func<'a,'b,bool>,name:string) =
        SpecBuilder<'a,'b>(generator0,generator1,assertion0,conditions,collects,(filter,name)::classifies)
//    member x.AndFor<'c>(generator:'c Gen, assertion:Func<'c,bool>) =
//        SpecBuilder<'a,'b,'c>   (generator0
//                                ,generator1
//                                ,generator
//                                ,fun a b c -> assertion0.Invoke(a,b) && assertion.Invoke(c)
//                                ,conditions |> List.map (fun f -> Func<'a,'b,'c,bool>(fun a b c -> f.Invoke(a,b)))
//                                ,collects |> List.map (fun f -> Func<'a,'b,'c,string>(fun a b c -> f.Invoke(a,b)))
//                                ,classifies |> List.map (fun (f,name) -> (Func<'a,'b,'c,bool>(fun a b c -> f.Invoke(a,b)),name))
//                                )
and SpecBuilder<'a,'b,'c>(generator0:'a Gen,generator1:'b Gen, generator2:'c Gen, 
                            assertion0:Func<'a,'b,'c,bool>,
                            conditions:Func<'a,'b,'c,bool> list, 
                            collects:Func<'a,'b,'c,string> list, 
                            classifies:(Func<'a,'b,'c,bool> * string) list) = 
    inherit UnbrowsableObject()
    override x.Build() =
            let conditions' a b c = conditions |> List.fold_left (fun s f -> s && f.Invoke(a,b,c)) true
            let collects' a b c prop = collects |> List.fold_left (fun prop f -> prop |> collect (f.Invoke(a,b,c))) prop
            let classifies' a b c prop = classifies |> List.fold_left (fun prop (f,name) -> prop |> classify (f.Invoke(a,b,c)) name) prop  
            forAll generator0 (fun a -> 
            forAll generator1 (fun b -> 
            forAll generator2 (fun c ->
                (conditions' a b c) ==> (assertion0.Invoke(a,b,c)) |> collects' a b c |> classifies' a b c))) 
                
                
type Spec =
    static member For<'a>(generator:'a Gen, assertion:Func<'a,bool>) =
        SpecBuilder<'a>(generator, assertion, [], [], [])
    static member For<'a,'b>(generator1:'a Gen,generator2:'b Gen, assertion:Func<'a,'b,bool>) =
        SpecBuilder<'a,'b>(generator1, generator2, assertion,[],[],[])

open Generator

[<System.Runtime.CompilerServices.Extension>]
type GeneratorExtensions = 
    [<System.Runtime.CompilerServices.Extension>]
    static member Select(g:Gen<_>, selector : Func<_,_>) = g.Map(fun a -> selector.Invoke(a))
    
    [<System.Runtime.CompilerServices.Extension>]
    static member Where(g:Gen<_>, predicate : Func<_,_>) = suchThat (fun a -> predicate.Invoke(a)) g
    
    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany(source:Gen<_>, f:Func<_, Gen<_>>) = 
        gen { let! a = source
              return! f.Invoke(a) }
    
    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany(source:Gen<_>, f:Func<_, Gen<_>>, select:Func<_,_,_>) =
        gen { let! a = source
              let! b = f.Invoke(a)
              return select.Invoke(a,b) }

init.Force()
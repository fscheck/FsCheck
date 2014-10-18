(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2013 Kurt Schelfthout. All rights reserved.          **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck.Fluent

open System
open System.Linq
open System.ComponentModel
open System.Collections.Generic
open FsCheck
open Common
open Arb
open Gen
open Testable
open Prop
open Runner

//TODO:
//Within -> rely on testing frameworks?
//Throws -> rely on testing frameworks?
//"And" and "Or" should start a new property, with own classifies and labels etc (see prop_Label)
//label: maybe add some overloads, should be able to nest (see propMul)

type WeightAndValue<'a>(weight:int,value:'a) =
    member x.Weight = weight
    member x.Value = value
    
type Any private() =
    static let regs = Runner.init.Force()

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///equal probability.
    static member private OneOfSeqGen gs = 
        gs |> Seq.toList |> oneof

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    static member private OneOfSeqValue vs = 
        vs |> Seq.toList |> elements

    ///Sequence the given list of generators into a generator of a list.
    static member private SequenceSeq<'a> gs = 
        gs |> Seq.toList |> sequence |> map (fun list -> new List<'a>(list))

    /// Returns a generator for the type argument
    static member OfType<'a>() = 
        from<'a>.Generator

    /// Generate a constant value
    static member Value (value) = 
        constant value

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    static member ValueIn (values : seq<_>) = 
        values |> Any.OneOfSeqValue

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    static member ValueIn ([<ParamArrayAttribute>] values : array<_>) = 
        values |> Any.OneOfSeqValue

    ///Generates an integer between l and h, inclusive.
    static member IntBetween (l,h) = 
        choose (l,h)

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    static member WeighedValueIn ( weighedValues : seq<WeightAndValue<'a>> ) =
        weighedValues |> Any.OneOfWeighedSeqValue

    ///Build a generator that generates a value from one of the constants in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    static member WeighedValueIn ( [<ParamArrayAttribute>] weighedValues : array<WeightAndValue<'a>> ) =
        weighedValues |> Any.OneOfWeighedSeqValue

    ///Build a generator that generates a value from one of the constants in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    static member private OneOfWeighedSeqValue ws = 
        ws |> Seq.map (fun wv -> (wv.Weight, constant wv.Value)) |> Seq.toList |> frequency

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///equal probability.
    static member GeneratorIn (generators : seq<Gen<_>>) = 
        generators |> Any.OneOfSeqGen

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///equal probability.
    static member GeneratorIn ([<ParamArrayAttribute>]  generators : array<Gen<_>>) = 
        generators |> Any.OneOfSeqGen

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    static member WeighedGeneratorIn ( weighedValues : seq<WeightAndValue<Gen<'a>>> ) =
        weighedValues |> Any.OneOfWeighedSeq

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    static member WeighedGeneratorIn ( [<ParamArrayAttribute>] weighedValues : array<WeightAndValue<Gen<'a>>> ) =
        weighedValues |> Any.OneOfWeighedSeq

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    static member private OneOfWeighedSeq ws = 
        ws |> Seq.map (fun wv -> (wv.Weight, wv.Value)) |> Seq.toList |> frequency

    ///Sequence the given list of generators into a generator of a list.
    static member SequenceOf<'a> (generators:seq<Gen<'a>>) = 
        generators |> Any.SequenceSeq

    ///Sequence the given list of generators into a generator of a list.
    static member SequenceOf<'a> ([<ParamArrayAttribute>]generators:array<Gen<'a>>) = 
        generators |> Any.SequenceSeq

    static member OfSize (sizedGen : Func<int,Gen<_>>) =
        sized <| fun s -> (sizedGen.Invoke(s))


type Shrink =
    ///Returns the immediate shrinks for the given value based on its type.
    static member Type<'a>() = shrink<'a>

//mutable counterpart of the Config type
type Configuration() =
    let mutable maxTest = Config.Quick.MaxTest
    let mutable maxFail = Config.Quick.MaxFail
    let mutable name = Config.Quick.Name
    let mutable every = Config.Quick.Every
    let mutable everyShrink = Config.Quick.EveryShrink
    let mutable startSize = Config.Quick.StartSize
    let mutable endSize = Config.Quick.EndSize
    let mutable runner = Config.Quick.Runner
    let mutable replay = Config.Quick.Replay

    ///The maximum number of tests that are run.
    member x.MaxNbOfTest with get() = maxTest and set(v) = maxTest <- v

    ///The maximum number of tests where values are rejected
    member x.MaxNbOfFailedTests with get() = maxFail and set(v) = maxFail <- v

    ///Name of the test.
    member x.Name with get() = name and set(v) = name <- v

    ///What to print when new arguments args are generated in test n
    member x.Every with get() = new Func<int,obj array,string>(fun i arr -> every i (Array.toList arr)) 
                   and set(v:Func<int,obj array,string>) = every <- fun i os -> v.Invoke(i,List.toArray os)

    ///What to print every time a counter-example is succesfully shrunk
    member x.EveryShrink with get() = new Func<obj array,string>(Array.toList >> everyShrink)
                         and set(v:Func<obj array,string>) = everyShrink <- fun os -> v.Invoke(List.toArray os)

    ///The size to use for the first test.
    member x.StartSize with get() = startSize and set(v) = startSize <- v

    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member x.EndSize with get() = endSize and set(v) = endSize <- v

    ///A custom test runner, e.g. to integrate with a test framework like xUnit or NUnit. 
    member x.Runner with get() = runner and set(v) = runner <- v

    //TODO: figure out how to deal with null values
    //member x.Replay with get() = (match replay with None -> null | Some s -> s) and set(v) = replay = Some v
    member internal x.ToConfig() =
        { MaxTest = maxTest
          MaxFail = maxFail 
          Name = name
          Every = every
          EveryShrink = everyShrink
          StartSize = startSize
          EndSize = endSize
          Runner = runner
          Replay = None
          Arbitrary = []
        }

[<AbstractClass>]
type UnbrowsableObject() =
    inherit obj()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override x.Equals(other) = base.Equals(other)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override x.GetHashCode() = base.GetHashCode()
//    [<EditorBrowsable(EditorBrowsableState.Never)>]
//    override x.GetType() = base.GetType() //GetType cannot be overridden
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    override x.ToString() = base.ToString()
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    abstract Build : unit -> Property

    /// Check one property with the quick configuration
    member x.QuickCheck() = Check.Quick(x.Build())

    /// Check one property with the quick configuration, and throw an exception if it fails or is exhausted.
    member x.QuickCheckThrowOnFailure() = Check.QuickThrowOnFailure(x.Build())

    /// Check one property with the verbose configuration.
    member x.VerboseCheck() = Check.Verbose(x.Build())

    /// Check one property with the quick configuration, and using the given name.
    member x.QuickCheck(name:string) = Check.Quick(name,x.Build())

    /// Check one property with the quick configuration, and throw an exception if it fails or is exhausted.
    member x.QuickCheckThrowOnFailure(name:string) = Check.QuickThrowOnFailure(name,x.Build())

    ///Check one property with the verbose configuration, and using the given name.
    member x.VerboseCheck(name:string) = Check.Verbose(name,x.Build())

    ///Check the given property using the given config.
    member x.Check(configuration:Configuration) = Check.One(configuration.ToConfig(),x.Build())

and SpecBuilder<'a> internal   ( generator0:'a Gen
                               , shrinker0: 'a -> 'a seq
                               , assertion0:'a -> Property
                               , conditions:('a -> bool) list
                               , collects:('a -> string) list
                               , classifies:(('a -> bool) * string) list) =
    inherit UnbrowsableObject()
    override x.Build() =
            let conditions' a = conditions |> List.fold (fun s f -> s && f a) true
            let collects' a prop = collects |> List.fold (fun prop f -> prop |> collect (f a)) prop
            let classifies' a prop = classifies |> List.fold (fun prop (f,name) -> prop |> classify (f a) name) prop  
            forAll (Arb.fromGenShrink(generator0,shrinker0)) (fun a -> (conditions' a) ==> lazy (assertion0 a) |> collects' a |> classifies' a)
    member x.When( condition:Func<'a,bool> ) = 
        SpecBuilder<'a>(generator0, shrinker0, assertion0, (fun a -> condition.Invoke(a))::conditions, collects, classifies)

    ///Collect data values. The argument of collect is evaluated in each test case, 
    ///and the distribution of values is reported.
    member x.Collect(collectedValue:Func<'a,string>)=
        SpecBuilder<'a>(generator0, shrinker0,assertion0,conditions,(fun a -> collectedValue.Invoke(a))::collects,classifies)

    member x.Classify(filter:Func<'a,bool>,name:string) =
        SpecBuilder<'a>(generator0, shrinker0,assertion0,conditions,collects,((fun a -> filter.Invoke(a)),name)::classifies)
    member x.Shrink(shrinker:Func<'a,'a seq>) =
        SpecBuilder<'a>( generator0, shrinker.Invoke, assertion0, conditions, collects, classifies)
    member x.Label( name:string ) =
        SpecBuilder<'a>(generator0, shrinker0, label name << assertion0,conditions, collects, classifies)

    ///Construct a property that succeeds if both succeed.
    member x.And(assertion : Func<'a,bool>) =
        SpecBuilder<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .&. (assertion.Invoke(a))), conditions, collects, classifies)

    ///Construct a property that succeeds if both succeed.
    member x.And(assertion : Func<'a,bool>, name:string ) =
        SpecBuilder<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .&. (label name (assertion.Invoke(a)))), conditions, collects, classifies)

    ///Construct a property that fails if both fail.
    member x.Or(assertion : Func<'a,bool>) =
        SpecBuilder<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .|. (assertion.Invoke(a))), conditions, collects, classifies)

    ///Construct a property that fails if both fail.
    member x.Or(assertion : Func<'a,bool>, name:string ) =
        SpecBuilder<'a>( generator0, shrinker0, (fun a -> (assertion0 a) .|. (label name (assertion.Invoke(a)))), conditions, collects, classifies)
    member x.AndFor<'b>(generator:'b Gen, assertion:Func<'b,bool>) =
        SpecBuilder<'a,'b>  (generator0
                            ,shrinker0 
                            ,generator
                            ,shrink
                            ,fun a b -> (assertion0 a) .&. property (assertion.Invoke(b))
                            ,conditions |> List.map (fun f -> (fun a b -> f a))
                            ,collects |> List.map (fun f -> (fun a b -> f a))
                            ,classifies |> List.map (fun (f,name) -> ((fun a b -> f a),name))
                            )
  
       
and SpecBuilder<'a,'b> internal   ( generator0:'a Gen
                                  , shrinker0: 'a -> 'a seq
                                  , generator1:'b Gen
                                  , shrinker1: 'b -> 'b seq
                                  , assertion0:'a -> 'b -> Property
                                  , conditions:('a -> 'b -> bool) list
                                  , collects:('a -> 'b -> string) list
                                  , classifies:(('a -> 'b -> bool) * string) list) = 
    inherit UnbrowsableObject()
    override x.Build() =
            let conditions' a b = conditions |> List.fold (fun s f -> s && f a b) true
            let collects' a b prop = collects |> List.fold (fun prop f -> prop |> collect (f a b)) prop
            let classifies' a b prop = classifies |> List.fold (fun prop (f,name) -> prop |> classify (f a b) name) prop  
            forAll (Arb.fromGen generator0) (fun a -> forAll (Arb.fromGen generator1) (fun b -> (conditions' a b) ==> lazy (assertion0 a b) |> collects' a b |> classifies' a b))
    member x.When( condition:Func<'a,'b,bool> ) = 
        SpecBuilder<'a,'b>(generator0, shrinker0, generator1, shrinker1, assertion0, (fun a b -> condition.Invoke(a,b))::conditions, collects, classifies)
    member x.Collect(collectedValue:Func<'a,'b,string>)=
        SpecBuilder<'a,'b>(generator0, shrinker0, generator1, shrinker1, assertion0,conditions,(fun a b -> collectedValue.Invoke(a,b))::collects,classifies)
    member x.Classify(filter:Func<'a,'b,bool>,name:string) =
        SpecBuilder<'a,'b>(generator0, shrinker0,generator1, shrinker1,assertion0,conditions,collects,((fun a b -> filter.Invoke(a,b)),name)::classifies)
    member x.Shrink(shrinker:Func<'b,'b seq>) =
        SpecBuilder<'a,'b>( generator0, shrinker0, generator1, shrinker.Invoke, assertion0, conditions, collects, classifies)
    member x.Label( name:string ) =
        SpecBuilder<'a,'b>(generator0, shrinker0, generator1, shrinker1, (fun a b-> label name (assertion0 a b)),conditions, collects, classifies)
    member x.And(assertion : Func<'a,'b,bool>) =
        SpecBuilder<'a,'b>( generator0, shrinker0, generator1, shrinker1,
            (fun a b -> (assertion0 a b) .&. (assertion.Invoke(a, b))) , conditions, collects, classifies)
    member x.And(assertion : Func<'a,'b,bool>, name:string ) =
        SpecBuilder<'a,'b>( generator0, shrinker0, generator1, shrinker1, 
            (fun a b -> (assertion0 a b) .&. (label name (assertion.Invoke(a,b)))), conditions, collects, classifies)
    member x.Or(assertion : Func<'a,'b,bool>) =
        SpecBuilder<'a,'b>( generator0, shrinker0, generator1, shrinker1, 
            (fun a b -> (assertion0 a b) .|. (assertion.Invoke(a,b))), conditions, collects, classifies)
    member x.Or(assertion : Func<'a,'b,bool>, name:string ) =
        SpecBuilder<'a,'b>( generator0, shrinker0, generator1, shrinker1, 
            (fun a b-> (assertion0 a b) .|. (label name (assertion.Invoke(a,b)))), conditions, collects, classifies)
    member x.AndFor<'c>(generator:'c Gen, assertion:Func<'c,bool>) =
        SpecBuilder<'a,'b,'c>   (generator0, shrinker0
                                ,generator1, shrinker1
                                ,generator, shrink
                                ,fun a b c -> (assertion0 a b) .&. property (assertion.Invoke(c))
                                ,conditions |> List.map (fun f -> (fun a b c -> f a b))
                                ,collects |> List.map (fun f -> (fun a b c -> f a b))
                                ,classifies |> List.map (fun (f,name) -> (fun a b c -> f a b),name)
                                )
                                
and SpecBuilder<'a,'b,'c> internal  ( generator0:'a Gen
                                    , shrinker0:'a -> 'a seq
                                    , generator1:'b Gen
                                    , shrinker1: 'b -> 'b seq
                                    , generator2:'c Gen
                                    , shrinker2: 'c -> 'c seq
                                    , assertion0:'a -> 'b -> 'c -> Property
                                    , conditions:('a -> 'b -> 'c -> bool) list
                                    , collects:('a -> 'b -> 'c -> string) list
                                    , classifies:(('a -> 'b -> 'c -> bool) * string) list) = 
    inherit UnbrowsableObject()
    override x.Build() =
            let conditions' a b c = conditions |> List.fold (fun s f -> s && f a b c) true
            let collects' a b c prop = collects |> List.fold (fun prop f -> prop |> collect (f a b c)) prop
            let classifies' a b c prop = classifies |> List.fold (fun prop (f,name) -> prop |> classify (f a b c) name) prop  
            forAll (Arb.fromGen generator0) (fun a -> 
            forAll (Arb.fromGen generator1) (fun b -> 
            forAll (Arb.fromGen generator2) (fun c ->
                (conditions' a b c) ==> lazy (assertion0 a b c) |> collects' a b c |> classifies' a b c))) 
    member x.When( condition:Func<'a,'b,'c,bool> ) = 
        SpecBuilder<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, assertion0, (fun a b c -> condition.Invoke(a,b,c))::conditions, collects, classifies)
    member x.Collect(collectedValue:Func<'a,'b,'c,string>)=
        SpecBuilder<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, assertion0, conditions,(fun a b c -> collectedValue.Invoke(a,b,c))::collects,classifies)
    member x.Classify(filter:Func<'a,'b,'c,bool>,name:string) =
        SpecBuilder<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, assertion0, conditions, collects,((fun a b c -> filter.Invoke(a,b,c)),name)::classifies)         
    member x.Shrink(shrinker:Func<'c,'c seq>) =
        SpecBuilder<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker.Invoke, assertion0, conditions, collects, classifies)
    member x.Label( name:string ) =
        SpecBuilder<'a,'b,'c>(generator0, shrinker0, generator1, shrinker1, generator2, shrinker2, (fun a b c -> label name (assertion0 a b c)),conditions, collects, classifies)
    member x.And(assertion : Func<'a,'b,'c,bool>) =
        SpecBuilder<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1,generator2, shrinker2,
            (fun a b c -> (assertion0 a b c) .&. (assertion.Invoke(a, b, c))) , conditions, collects, classifies)
    member x.And(assertion : Func<'a,'b,'c,bool>, name:string ) =
        SpecBuilder<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1, generator2, shrinker2,
            (fun a b c -> (assertion0 a b c) .&. (label name (assertion.Invoke(a,b,c)))), conditions, collects, classifies)
    member x.Or(assertion : Func<'a,'b,'c,bool>) =
        SpecBuilder<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1, generator2, shrinker2,
            (fun a b c -> (assertion0 a b c) .|. (assertion.Invoke(a,b,c))), conditions, collects, classifies)
    member x.Or(assertion : Func<'a,'b,'c,bool>, name:string ) =
        SpecBuilder<'a,'b,'c>( generator0, shrinker0, generator1, shrinker1,generator2, shrinker2, 
            (fun a b c -> (assertion0 a b c) .|. (label name (assertion.Invoke(a,b,c)))), conditions, collects, classifies)  
      
                
type Spec() =
    static let _ = Runner.init.Value
    static member ForAny(assertion:Func<'a,bool>) =
        Spec.For(Any.OfType<'a>(),assertion)
    static member ForAny(assertion:Action<'a>) =
        Spec.For(Any.OfType<'a>(),assertion)
    static member ForAny(assertion:Func<'a,'b,bool>) =
        Spec.For(Any.OfType<'a>(),Any.OfType<'b>(),assertion)
    static member ForAny(assertion:Func<'a,'b,'c,bool>) =
        Spec.For(Any.OfType<'a>(),Any.OfType<'b>(),Any.OfType<'c>(),assertion)
    static member ForAny(assertion:Action<'a,'b>) =
        Spec.For(Any.OfType<'a>(),Any.OfType<'b>(),assertion)
        
    static member For(generator:'a Gen, assertion:Func<'a,bool>) =
        SpecBuilder<'a>(generator, shrink, property << assertion.Invoke, [], [], [])
    static member For(generator:'a Gen, assertion:Action<'a>) =
        SpecBuilder<'a>(generator, shrink, property << assertion.Invoke, [], [], [])
    static member For(generator1:'a Gen,generator2:'b Gen, assertion:Func<'a,'b,bool>) =
        SpecBuilder<'a,'b>(generator1, shrink, generator2, shrink, (fun a b -> property <| assertion.Invoke(a,b)),[],[],[])
    static member For(generator1:'a Gen,generator2:'b Gen,generator3:'c Gen, assertion:Func<'a,'b,'c,bool>) =
        SpecBuilder<'a,'b,'c>(generator1, shrink, generator2, shrink, generator3, shrink, (fun a b c -> property <| assertion.Invoke(a,b,c)),[],[],[])
    static member For(generator1:'a Gen,generator2:'b Gen, assertion:Action<'a,'b>) =
        SpecBuilder<'a,'b>(generator1, shrink, generator2, shrink, (fun a b -> property <| assertion.Invoke(a,b)),[],[],[])

open Gen

[<System.Runtime.CompilerServices.Extension>]
type GeneratorExtensions = 
    ///Map the given function to the value in the generator, yielding a new generator of the result type.  
    [<System.Runtime.CompilerServices.Extension>]
    static member Select(g:Gen<_>, selector : Func<_,_>) = g.Map(fun a -> selector.Invoke(a))

    ///Generates a value that satisfies a predicate. This function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high chance that 
    ///the predicate is satisfied.
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

    /// Generates a list of random length. The maximum length depends on the
    /// size parameter.
    [<System.Runtime.CompilerServices.Extension>]
    static member MakeList<'a> (generator) = listOf generator |> map (fun list -> new List<'a>(list))

    /// Generates a non-empty list of random length. The maximum length 
    /// depends on the size parameter.
    [<System.Runtime.CompilerServices.Extension>]
    static member MakeNonEmptyList<'a> (generator) = nonEmptyListOf generator |> map (fun list -> new List<'a>(list))
    
    ///Generates a list of given length, containing values generated by the given generator.
    [<System.Runtime.CompilerServices.Extension>]
    static member MakeListOfLength<'a> (generator, count) = listOfLength count generator |> map (fun list -> new List<'a>(list))
    
    ///Override the current size of the test. resize n g invokes generator g with size parameter n.
    [<System.Runtime.CompilerServices.Extension>]
    static member Resize (generator, sizeTransform : Func<int,int>) =
        sized <| fun s -> resize (sizeTransform.Invoke(s)) generator

    /// Construct an Arbitrary instance from a generator.
    /// Shrink is not supported for this type.
    [<System.Runtime.CompilerServices.Extension>]
    static member ToArbitrary generator =
        Arb.fromGen generator

[<System.Runtime.CompilerServices.Extension>]
type ArbitraryExtensions =
    ///Construct an Arbitrary instance for a type that can be mapped to and from another type (e.g. a wrapper),
    ///based on a Arbitrary instance for the source type and two mapping functions. 
    [<System.Runtime.CompilerServices.Extension>]
    static member Convert (arb, convertTo: Func<_,_>, convertFrom: Func<_,_>) =
        Arb.convert convertTo.Invoke convertFrom.Invoke arb
    
type DefaultArbitraries =
    ///Register the generators that are static members of the type argument.
    static member Add<'t>() = Arb.register<'t>()
    //static member Overwrite<'t>() = Gen.overwrite<'t>()
  
//do init.Value
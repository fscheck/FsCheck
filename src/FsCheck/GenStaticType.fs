namespace FsCheck

open Gen
open System
open System.Linq
open System.Collections.Generic

///2-tuple containing a weight and a value, used in some Gen methods to indicate
///the probability of a value.
[<NoComparison>]
type WeightAndValue<'a> =
    { Weight: int
      Value : 'a  
    }
 
///Methods to build random value generators.
[<AbstractClass; Sealed>]
type Gen private() =

    ///Always generate value.
    ///[category: Creating generators] 
    static member Constant (value) = 
        constant value

    ///Build a generator that randomly generates one of the values in the given non-empty IEnumerable.
    ///[category: Creating generators]
    static member Elements (values : seq<_>) = 
        values |> elements

    ///Build a generator that randomly generates one of the given values.
    ///[category: Creating generators]
    static member Elements ([<ParamArrayAttribute>] values : array<_>) = 
        values |> elements

    ///Generates an integer between l and h, inclusive.
    ///[category: Creating generators]
    static member Choose (l,h) = 
        choose (l,h)

    ///Build a generator that generates a value from one of the given generators, with
    ///equal probability.
    ///[category: Creating generators from generators]
    static member OneOf (generators : seq<Gen<_>>) = 
        generators |> oneof

    ///Build a generator that generates a value from one of the given generators, with
    ///equal probability.
    ///[category: Creating generators from generators]
    static member OneOf ([<ParamArrayAttribute>]  generators : array<Gen<_>>) = 
        generators |> oneof

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    ///[category: Creating generators from generators]
    static member Frequency ( weighedValues : seq<WeightAndValue<Gen<'a>>> ) =
        weighedValues |> Gen.FrequencyOfWeighedSeq

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    ///[category: Creating generators from generators]
    static member Frequency ( [<ParamArrayAttribute>] weighedValues : array<WeightAndValue<Gen<'a>>> ) =
        weighedValues |> Gen.FrequencyOfWeighedSeq

    static member private FrequencyOfWeighedSeq ws = 
        ws |> Seq.map (fun wv -> (wv.Weight, wv.Value)) |> frequency

    ///Sequence the given list of generators into a generator of a list.
    ///[category: Creating generators from generators]
    static member Sequence<'a> (generators:seq<Gen<'a>>) = 
        generators |> sequence |> map (fun list -> list :> IEnumerable<'a>)

    ///Sequence the given list of generators into a generator of a list.
    ///[category: Creating generators from generators]
    static member Sequence<'a> ([<ParamArrayAttribute>]generators:array<Gen<'a>>) = 
        generators |> sequence |> map (fun list -> list.ToArray())

    /// Generates sublists of the given IEnumerable.
    ///[category: Creating generators]
    static member SubListOf s = 
        subListOf s
        |> map (fun l -> new List<_>(l) :> IList<_>)

    /// Generates sublists of the given arguments.
    ///[category: Creating generators]
    static member SubListOf ([<ParamArrayAttribute>] s:_ array) = 
        subListOf s
        |> map (fun l -> new List<_>(l) :> IList<_>)

    ///Obtain the current size. sized g calls g, passing it the current size as a parameter.
    ///[category: Managing size]
    static member Sized (sizedGen : Func<int,Gen<_>>) =
        sized <| fun s -> (sizedGen.Invoke(s))


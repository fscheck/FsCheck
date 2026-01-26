namespace FsCheck.Fluent

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

open FsCheck
open FsCheck.FSharp
open FsCheck.Internals

[<AbstractClass; Sealed; Extension>]
type Gen = 
    /// Always generate the same given value. See also Fresh.
    //[category: Creating generators]
    static member Constant(value:'T) = 
        Gen.constant value

    /// Generate fresh instances by calling create every time the generator 
    /// generates a new value. Useful for generating new instances of mutable
    /// objects.
    /// See also constant.
    //[category: Creating generators]
    static member Fresh(create:Func<'T>) = 
        if isNull create then nullArg "create"
        Gen.fresh create.Invoke

    /// Obtain the current size. Sized(g) calls g, passing it the current size as a parameter.
    //[category: Managing size]
    static member Sized(createWithSize:Func<int,Gen<'T>>) : Gen<'T> =
        Gen.sized createWithSize.Invoke

    /// Generates integers between l and h, inclusive.
    //[category: Creating generators]
    static member Choose(l:int, h:int) =
        Gen.choose(l,h)

    ///Build a generator that randomly generates one of the values in the given non-empty, finite sequence.
    //[category: Creating generators]
    static member Elements (elements:seq<'T>) =
        Gen.elements elements

    ///Build a generator that randomly generates one of the values in the given non-empty array.
    //[category: Creating generators]
    static member Elements ([<ParamArray>]elements:'T[]) =
        Gen.Elements(elements :> seq<_>)

    /// Build a generator that takes a non-empty sequence and randomly generates
    /// one of the values among an initial segment of that sequence. The size of
    /// this initial segment increases with the size parameter. Essentially this
    /// generator is Gen.Elements but taking also the size into account.
    //[category: Creating generators]
    static member GrowingElements (elements:seq<'T>) =
        Gen.growingElements elements

    /// Build a generator that takes a non-empty array and randomly generates
    /// one of the values among an initial segment of that array. The size of
    /// this initial segment increases with the size parameter. Essentially this
    /// generator is Gen.Elements but taking also the size into account.
    //[category: Creating generators]
    static member GrowingElements ([<ParamArray>]elements:'T[]) =
        Gen.GrowingElements(elements :> seq<_>)

    /// Build a generator that generates a value from one of the generators in the given non-empty sequence,
    /// with equal probability.
    //[category: Creating generators from generators]
    static member OneOf (gens:seq<Gen<'T>>) =
        Gen.oneof gens

    /// Build a generator that generates a value from one of the generators in the given non-empty array,
    /// with equal probability.
    //[category: Creating generators from generators]
    static member OneOf ([<ParamArray>]gens:array<Gen<'T>>) =
        Gen.OneOf(gens :> seq<_>)

    /// <summary>
    /// Build a generator that generates a value from one of the generators in the given non-empty seq, with
    /// given probabilities. The sum of the probabilities must be larger than zero.
    /// </summary>
    /// <param name="dist">Sequence of tuples where each tuple contains a weight and a generator.</param>
    /// <exception cref="System.ArgumentException">Thrown if the sum of the probabilities is less than or equal to 0.</exception>
    //[category: Creating generators from generators]
    static member Frequency (dist:seq<struct(int*Gen<'T>)>) =
        dist
        |> Seq.map Common.ofValue
        |> Gen.frequency 

    /// <summary>
    /// Build a generator that generates a value from one of the generators in the given non-empty arrat, with
    /// given probabilities. The sum of the probabilities must be larger than zero.
    /// </summary>
    /// <param name="dist">Sequence of tuples where each tuple contains a weight and a generator.</param>
    /// <exception cref="System.ArgumentException">Thrown if the sum of the probabilities is less than or equal to 0.</exception>
    //[category: Creating generators from generators]
    static member Frequency ([<ParamArray>]dist:array<struct(int*Gen<'T>)>) =
        Gen.Frequency(dist :> seq<_>) 

    /// Transform the given sequence of generators into a generator of a List.
    ///[category: Create generators from generators]
    static member CollectToList (source:seq<Gen<'T>>) =
        source
        |> Gen.sequenceToArray
        |> Gen.map (fun arr -> List<_>(arr))

    /// Transform the given sequence into a generator of a List using the given function 
    /// to create a generator for each element of the sequence.
    ///[category: Create generators from generators]
    static member CollectToList (source:seq<'T>, createGen:Func<'T,Gen<'U>>) =
        source
        |> Gen.collectToArray createGen.Invoke
        |> Gen.map (fun arr -> List<_>(arr))

    /// Transform the given sequence of generators into a generator of an array.
    ///[category: Create generators from generators]
    static member CollectToArray (source:seq<Gen<'T>>) =
        source
        |> Gen.sequenceToArray

    /// Transform the given sequence into a generator of an array using the given function 
    /// to create a generator for each element of the sequence.
    ///[category: Create generators from generators]
    static member CollectToArray (source:seq<'T>, createGen:Func<'T,Gen<'U>>) =
        source
        |> Gen.collectToArray createGen.Invoke

     /// Transform the given sequence of generators into a generator of an array.
    /// Each sequence generated by the resulting generator can be infinite, if the source sequence is infinite.
    ///[category: Create generators from generators]
    static member CollectToSequence (source:seq<Gen<'T>>) =
        source
        |> Gen.sequenceToSeq

    /// Transform the given sequence into a generator of sequences using the given function 
    /// to create a generator for each element of the sequence.
    /// Each sequence generated by the resulting generator can be infinite, if the source sequence is infinite.
    ///[category: Create generators from generators]
    static member CollectToSequence (source:seq<'T>, createGen:Func<'T,Gen<'U>>) =
        source
        |> Gen.collectToSeq createGen.Invoke

    /// Generates random permutations of the given sequence.
    //[category: Creating generators]
    static member Shuffle (source:seq<'T>) = 
        Gen.shuffle source

    /// Generates random permutations of the given array.
    //[category: Creating generators]
    static member Shuffle ([<ParamArray>]source:array<'T>) = 
        Gen.Shuffle(source :> seq<_>)

    /// Generates random arrays of given length where the sum of
    /// all elements equals the given sum.
    //[category: Creating generators]
    static member Piles(length, sum) =
        Gen.piles length sum

    /// Generates sublists of the given sequence. For a given list of length n,
    /// each sublist has between 0 and n elements, and the order of the 
    /// elements is the same as in the given sequence.
    //[category: Creating generators]
    static member SubListOf(lst:seq<'T>) :Gen<List<'T>> =
        Gen.subListOf lst
        |> Gen.map List<_>

    /// Generates sublists of the given array. For a given list of length n,
    /// each sublist has between 0 and n elements, and the order of the 
    /// elements is the same as in the given sequence.
    //[category: Creating generators]
    static member SubListOf([<ParamArray>]source:array<'T>) :Gen<List<'T>> =
        Gen.SubListOf(source :> seq<_>)

    /// No-op, added to allow type annotations in LINQ expressions, e.g. from T x in e
    [<Extension>]
    static member Cast(gen:Gen<'T>) = gen

    /// Create a new generator by applying selector to each value in the given generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Select(source:Gen<'T>, selector : Func<'T,'U>) = 
        if isNull selector then nullArg "selector"
        source |> Gen.map selector.Invoke

    /// Creates a new generator that generates values from the source generator, 
    /// applies the selector to them, and generates values from the resulting generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member SelectMany(source:Gen<'T>, selector:Func<'T, Gen<'U>>) =
        if isNull selector then nullArg "selector" 
        source |> Gen.bind selector.Invoke
    
    /// Creates a new generator that generates values from the source generator, 
    /// applies the selector to them to get a new generator, and generates values 
    /// by applying the resultSelector to the source value and the value from the second
    /// generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member SelectMany(source:Gen<'T>, selector:Func<'T, Gen<'U>>, resultSelector:Func<_,_,'TResult>) =
        if isNull selector then nullArg "selector"
        if isNull resultSelector then nullArg "resultSelector"
        gen { let! a = source
              let! b = selector.Invoke(a)
              return resultSelector.Invoke(a,b) }

    /// Override the current size.
    //[category: Managing size]
    [<Extension>]
    static member Resize (generator, newSize) :Gen<'T> =
        Gen.resize newSize generator

    ///Modify a size using the given function before passing it to the given generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member ScaleSize(generator:Gen<'T>, scaleFunc:Func<int,int>) =
        Gen.scaleSize scaleFunc.Invoke generator

    /// Generates numberOfSample values with a new seed and size 50.
    //[category: Generating test values]
    [<Extension>]
    static member Sample(generator:Gen<'T>, numberOfSamples) =
        Gen.sample numberOfSamples generator

    /// Generates numberOfSample values with the given size.
    //[category: Generating test values]
    [<Extension>]
    static member Sample(generator:Gen<'T>, numberOfSamples, size) =
        Gen.sampleWithSize size numberOfSamples generator

    /// Generates numberOfSample values with the given seed and of the given size.
    //[category: Generating test values]
    [<Extension>]
    static member Sample(generator:Gen<'T>, numberOfSamples, seed:Rnd, size) =
        Gen.sampleWithSeed seed size numberOfSamples generator

    /// Build a generator that generates a 2-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Two (g:Gen<'T>) = Gen.map2 (fun a b -> struct (a,b)) g g

    /// Build a generator that generates a 3-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Three (g:Gen<'T>) = Gen.map3 (fun a b c -> struct (a,b,c)) g g g

    /// Build a generator that generates a 4-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Four (g:Gen<'T>) = Gen.map4 (fun a b c d -> struct (a,b,c,d)) g g g g

    ///Combine two generators into a generator of pairs.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Zip(first:Gen<'T1>,second:Gen<'T2>) =
        Gen.map2 (fun f s -> struct (f,s)) first second 

    ///Combine two generators into a new generator of the result of the given result selector.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Zip(first:Gen<'T1>,second:Gen<'T2>, resultSelector : Func<_, _, 'TResult>) =
        if isNull resultSelector then nullArg "resultSelector"
        Gen.map2 (fun f s -> resultSelector.Invoke(f,s)) first second

    /// Generates only values from the source generator that satisfy the predicate. This function keeps re-trying
    /// by increasing the size of the original generator ad infinitum.  Make sure there is a high chance that 
    /// the predicate is satisfied.
    [<Extension>]
    static member Where(source:Gen<'T>, predicate : Func<_,_>) = 
        if isNull predicate then nullArg "predicate"
        source |> Gen.where predicate.Invoke

    /// Tries to apply the given chooser function to successive values generated by the source generator, 
    /// returning the first result where the function returns a value. This function 'gives up' by generating null
    /// if the source generator did not generate any values for which the chooser function returned a value,
    /// after trying to get values by increasing its size.
    [<Extension>]
    static member TryPick(source:Gen<'T>, chooser : Func<'T, 'U>) = 
        if isNull chooser then nullArg "chooser"
        source |> Gen.tryPick (fun x -> 
            let result = chooser.Invoke(x)
            // Boxing allows null checking for both reference and value types
            if isNull (box result) then None else Some result)
        |> Gen.map (function Some v -> v | None -> Unchecked.defaultof<'U>)

    /// Applies the given chooser function to successive values generated by the source generator, 
    /// returning the first result where the function returns a non-null value. Contrary to TryPick, 
    /// this function keeps re-trying by increasing the size of the original generator ad infinitum. 
    /// Make sure there is a high probability that the chooser function returns a non-null value for some values.
    [<Extension>]
    static member Pick(source:Gen<'T>, chooser : Func<'T, 'U>) = 
        if isNull chooser then nullArg "chooser"
        source |> Gen.pick (fun x -> 
            let result = chooser.Invoke(x)
            // Boxing allows null checking for both reference and value types
            if isNull (box result) then None else Some result)

    ///Generates a list of given length, containing values generated by the given generator.
    //[category: Creating generators from generators]
    [<Extension>]
    static member ListOf (elementGen:Gen<'T>, nbOfElements) =
        Gen.arrayOfLength nbOfElements elementGen
        |> Gen.map List<_>

    /// Generates a list of random length. The maximum length depends on the
    /// size parameter.
    //[category: Creating generators from generators]
    [<Extension>]
    static member ListOf (elementGen:Gen<'T>) =
        Gen.arrayOf elementGen
        |> Gen.map List<_>

    /// Generates a non-empty list of random length. The maximum length 
    /// depends on the size parameter.
    //[category: Creating generators from generators]
    [<Extension>]
    static member NonEmptyListOf (elementGen:Gen<'T>) = 
        Gen.nonEmptyListOf elementGen 
        |> Gen.map List<_>
    
    /// Generates an array of a specified length.
    //[category: Creating generators from generators]
    [<Extension>]
    static member ArrayOf (elementGen:Gen<'T>, length) =
        Gen.arrayOfLength length elementGen

    /// Generates an array using the specified generator. 
    /// The maximum length is size+1.
    //[category: Creating generators from generators]
    [<Extension>]
    static member ArrayOf (elementGen:Gen<'T>) =
        Gen.arrayOf elementGen

    /// Generates a 2D array of the given dimensions.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Array2DOf (elementGen:Gen<'T>, rows, cols) =
        Gen.array2DOfDim rows cols elementGen

    /// Generates a 2D array. The square root of the size is the maximum number of rows and columns.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Array2DOf (elementGen:Gen<'T>) =
        Gen.array2DOf elementGen

    /// Build a generator that generates a value from two generators with equal probability.
    //[category: Creating generators from generators]
    [<Extension>]
    static member Or (generator:Gen<'T>, other) =
        Gen.oneof [ generator; other ]

    /// Build a generator that generates a value or `null` 1/8th of the time.
    //[category: Creating generators from generators]
    [<Extension>]
    static member OrNull (generator:Gen<'T>) =
        Gen.frequency [ (7, generator); (1, Gen.constant null) ]






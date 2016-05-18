(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

open Random

type internal IGen = 
    abstract AsGenObject : Gen<obj>
    
///Generator of a random value, based on a size parameter and a randomly generated int.
and [<NoEquality;NoComparison>] Gen<'a> = 
    private Gen of (int -> StdGen -> 'a)
        ///map the given function to the value in the generator, yielding a new generator of the result type.
        member internal x.Map<'a,'b> (f: 'a -> 'b) : Gen<'b> = match x with (Gen g) -> Gen (fun n r -> f (g n r))
    interface IGen with
        member x.AsGenObject = x.Map box

//private interface for reflection
type internal IArbitrary =
    abstract GeneratorObj : Gen<obj>
    abstract ShrinkerObj : obj -> seq<obj>

[<AbstractClass>]
type Arbitrary<'a>() =
    ///Returns a generator for 'a.
    abstract Generator      : Gen<'a>
    ///Returns a sequence of the immediate shrinks of the given value. The immediate shrinks should not include
    ///doubles or the given value itself. The default implementation returns the empty sequence (i.e. no shrinking).
    abstract Shrinker       : 'a -> seq<'a>
    default __.Shrinker _ = 
        Seq.empty
    interface IArbitrary with
        member x.GeneratorObj = (x.Generator :> IGen).AsGenObject
        member x.ShrinkerObj (o:obj) : seq<obj> =
            let tryUnbox v =
                try
                    Some (unbox v)
                with
                    | _ -> None

            match tryUnbox o with
                | Some v -> x.Shrinker v |> Seq.map box
                | None -> Seq.empty



///Computation expression builder for Gen.
[<AutoOpen>]
module GenBuilder =

    let private result x = Gen (fun _ _ -> x)

    let private bind ((Gen m) : Gen<_>) (k : _ -> Gen<_>) : Gen<_> = 
        Gen (fun n r0 -> let r1,r2 = split r0
                         let (Gen m') = k (m n r1) 
                         m' n r2)

    let private delay (f : unit -> Gen<_>) : Gen<_> = 
        Gen (fun n r -> match f() with (Gen g) -> g n r)

    let rec private doWhile p (Gen m) =
        let rec go pred size rand =
            if pred() then
                let r1,r2 = split rand
                m size r1 |> ignore
                go pred size r2 
            else
                ()
        Gen (fun n r -> go p n r)

    let private tryFinally (Gen m) handler = 
        Gen (fun n r -> try m n r finally handler ())

    let private dispose (x: #System.IDisposable) = x.Dispose()

    let private using r f = tryFinally (f r) (fun () -> dispose r)

    ///The workflow type for generators.
    type GenBuilder internal() =
        member __.Return(a) : Gen<_> = result a
        member __.Bind(m, k) : Gen<_> = bind m k
        member __.Delay(f) : Gen<_> = delay f
        member __.Combine(m1, m2) = bind m1 (fun () -> m2)
        member __.TryFinally(m, handler) = tryFinally m handler
        member __.TryWith(Gen m, handler) = Gen (fun n r -> try m n r with e -> handler e)
        member __.Using (a, k) =  using a k
        member __.ReturnFrom (g:Gen<_>) = g
        member __.While(p, m:Gen<_>) = doWhile p m
        member __.For(s:#seq<_>, f:('a -> Gen<'b>)) =
          using (s.GetEnumerator()) (fun ie ->
            doWhile (fun () -> ie.MoveNext()) (delay (fun () -> f ie.Current))
          )
        member __.Zero() = result ()

    ///The workflow function for generators, e.g. gen { ... }
    let gen = GenBuilder()

///2-tuple containing a weight and a value, used in some Gen methods to indicate
///the probability of a value.
[<NoComparison>]
type WeightAndValue<'a> =
    { Weight: int
      Value : 'a
    }

///Combinators to build custom random generators for any type.
module Gen =

    open Common
    open Random
    open System
    open System.Collections.Generic
    open System.ComponentModel

    ///Apply the function f to the value in the generator, yielding a new generator.
    //[category: Creating generators from generators]
    [<CompiledName("Map"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map f (gen:Gen<_>) = gen.Map f

    ///Obtain the current size. sized g calls g, passing it the current size as a parameter.
    //[category: Managing size]
    [<CompiledName("Sized"); EditorBrowsable(EditorBrowsableState.Never)>]
    let sized fgen = Gen (fun n r -> let (Gen m) = fgen n in m n r)

    ///Obtain the current size. sized g calls g, passing it the current size as a parameter.
    //[category: Managing size]
    [<CompiledName("Sized"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let sizedFunc (sizedGen : Func<int,Gen<_>>) =
        sized sizedGen.Invoke

    ///Override the current size of the test. resize n g invokes generator g with size parameter n.
    //[category: Managing size]
    [<CompiledName("Resize")>]
    let resize newSize (Gen m) = Gen (fun _ r -> m newSize r)

    ///Generates a value with maximum size n.
    //[category: Generating test values]
    [<CompiledName("Eval")>]
    let eval n rnd (Gen m) = 
        let size,rnd' = range (0,n) rnd
        m size rnd'

    ///Generates n values of the given size.
    //[category: Generating test values]
    [<CompiledName("Sample")>]
    let sample size n gn  = 
        let rec sample i seed samples =
            if i = 0 then samples
            else sample (i-1) (Random.stdSplit seed |> snd) (eval size seed gn :: samples)
        sample n (Random.newSeed()) []

    ///Generates an integer between l and h, inclusive.
    //[category: Creating generators]
    [<CompiledName("Choose")>]
    let choose (l, h) = Gen (fun _ r -> range (l,h) r |> fst) 

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    //[category: Creating generators]
    [<CompiledName("Elements")>]
    let elements xs = 
        choose (0, (Seq.length xs)-1) |> map (flip Seq.nth xs)

    [<CompiledName("GrowingElements")>]
    ///Build a generator that takes a non-empty sequence and randomly generates
    ///one of the values among an initial segment of that sequence. The size of
    ///this initial segment increases with the size parameter. Essentially this
    ///generator is Gen.elements but taking also the runtime size into account.
    //[category: Creating generators]
    let growingElements xs =
        let arr = Seq.toArray xs
        sized (fun s ->
            let s' = max 1 s
            let n  = min arr.Length s'
            elements (arr |> Seq.take n))

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    //[category: Creating generators]
    [<CompiledName("Elements"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let elementsArr ([<ParamArrayAttribute>] values : array<_>) = 
        values |> elements

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///equal probability.
    //[category: Creating generators from generators]
    [<CompiledName("OneOf")>]
    let oneof gens = gen.Bind(elements gens, id)

    ///Build a generator that generates a value from one of the given generators, with
    ///equal probability.
    //[category: Creating generators from generators]
    [<CompiledName("OneOf"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let oneOfArr ([<ParamArrayAttribute>]  generators : array<Gen<_>>) = 
        generators |> oneof

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    ///<exception cref="System.ArgumentException">Thrown if the sum of the probabilities is less than or equal to 0.</exception>
    //[category: Creating generators from generators]
    [<CompiledName("Frequency")>]
    let frequency xs =
        let xs = Seq.toArray xs
        let tot = Array.sumBy fst xs
        let rec pick i n =
            let k,x = xs.[i]
            if n<=k then x else pick (i+1) (n-k)
        if tot <= 0 then 
            invalidArg "xs" "Frequency was called with a sum of probabilities less than or equal to 0. No elements can be generated."
        else
            gen.Bind(choose (1,tot), pick 0)

    let private frequencyOfWeighedSeq ws = 
        ws |> Seq.map (fun wv -> (wv.Weight, wv.Value)) |> frequency

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    //[category: Creating generators from generators]
    [<CompiledName("Frequency"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let frequencySeqWeightAndValue ( weightedValues : seq<WeightAndValue<Gen<'a>>> ) =
        weightedValues |> frequencyOfWeighedSeq

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    //[category: Creating generators from generators]
    [<CompiledName("Frequency"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let frequencyWeightAndValueArr ( [<ParamArrayAttribute>] weightedValues : WeightAndValue<Gen<'a>>[] ) =
        weightedValues |> frequencyOfWeighedSeq

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    //[category: Creating generators from generators]
    [<CompiledName("Frequency"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let frequencyTupleArr ( [<ParamArrayAttribute>] weightedValues : (int * Gen<'a>)[] ) =
        weightedValues |> frequency

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map2"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map2 f = fun a b -> gen {   let! a' = a
                                    let! b' = b
                                    return f a' b' }
                                        
    ///Build a generator that generates a 2-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Two")>]
    let two g = map2 (fun a b -> (a,b)) g g

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map3"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map3 f = fun a b c -> gen { let! a' = a
                                    let! b' = b
                                    let! c' = c
                                    return f a' b' c' }

    ///Build a generator that generates a 3-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Three")>]
    let three g = map3 (fun a b c -> (a,b,c)) g g g

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map4"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map4 f = fun a b c d -> gen {   let! a' = a
                                        let! b' = b
                                        let! c' = c
                                        let! d' = d
                                        return f a' b' c' d' }

    ///Build a generator that generates a 4-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Four")>]
    let four g = map4 (fun a b c d -> (a,b,c,d)) g g g g

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map5"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map5 f = fun a b c d e -> gen {  let! a' = a
                                         let! b' = b
                                         let! c' = c
                                         let! d' = d
                                         let! e' = e
                                         return f a' b' c' d' e'}

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map6"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map6 f = fun a b c d e g -> gen {   let! a' = a
                                            let! b' = b
                                            let! c' = c
                                            let! d' = d
                                            let! e' = e
                                            let! g' = g
                                            return f a' b' c' d' e' g'}

    ///Sequence the given seq of generators into a generator of a list.
    //[category: Creating generators from generators]
    [<CompiledName("SequenceToList"); EditorBrowsable(EditorBrowsableState.Never)>]
    let sequence l = 
        let rec go gs acc size r0 = 
            match gs with
            | [] -> List.rev acc
            | (Gen g)::gs' ->
                let r1,r2 = split r0
                let y = g size r1
                go gs' (y::acc) size r2
        Gen(fun n r -> go (Seq.toList l) [] n r)

    ///Sequence the given list of generators into a generator of a list.
    //[category: Creating generators from generators]
    [<CompiledName("Sequence"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let sequenceToSeq generators = 
        generators |> sequence |> map List.toSeq

    ///Sequence the given list of generators into a generator of a list.
    //[category: Creating generators from generators]
    [<CompiledName("Sequence"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let sequenceToArr ([<ParamArrayAttribute>]generators:array<Gen<_>>) = 
        generators |> sequence |> map List.toArray

    ///Generates a list of given length, containing values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("ListOf")>]
    let listOfLength n arb = sequence [ for _ in 1..n -> arb ]

    ///Generates a random permutation of the given sequence.
    //[category: Creating generators]
    [<CompiledName("Shuffle")>]
    let shuffle xs =
        // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
        let xs = xs |> Seq.toArray
        let swap (arr : _ array) i j =
            let v = arr.[j]
            arr.[j] <- arr.[i]
            arr.[i] <- v
        gen {
            let copy = Array.copy xs
            let maxI = copy.Length - 1
            let! indexes = [ for i in 0..maxI - 1 -> choose (i, maxI) ]
                           |> sequence
            indexes |> List.iteri (swap copy)
            return copy
        }

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values from by increasing its size.
    //[category: Creating generators from generators]
    [<CompiledName("SuchThatOption"); EditorBrowsable(EditorBrowsableState.Never)>]
    let suchThatOption p gn =
        let rec tryValue k n =
            match (k,n) with 
            | (_,0) -> gen {return None }
            | (k,n) -> gen {let! x = resize (2*k+n) gn
                            if p x then return Some x else return! tryValue (k+1) (n-1) }
        sized (tryValue 0 << max 1)


    ///Generates a value that satisfies a predicate. Contrary to suchThatOption, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high probability that 
    ///the predicate is satisfied.
    //[category: Creating generators from generators]
    [<CompiledName("SuchThat")>]
    let rec suchThat predicate generator =
        gen {   let! mx = suchThatOption predicate generator
                match mx with
                | Some x    -> return x
                | None      -> return! sized (fun n -> resize (n+1) (suchThat predicate generator)) }

    ///Generates a value that satisfies a predicate. Contrary to tryWhere, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high probability that 
    ///the predicate is satisfied.
    //[category: Creating generators from generators]
    [<CompiledName("Where");EditorBrowsable(EditorBrowsableState.Never)>]
    let where predicate generator = suchThat predicate generator

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values from by increasing its size.
    //[category: Creating generators from generators]
    [<CompiledName("TryWhere"); EditorBrowsable(EditorBrowsableState.Never)>]
    let tryWhere = suchThatOption
    
    ///Generates a value that satisfies a predicate. Contrary to tryFilter, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high probability that 
    ///the predicate is satisfied.
    //[category: Creating generators from generators]
    [<CompiledName("Filter")>]
    let filter = suchThat

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values from by increasing its size.
    //[category: Creating generators from generators]
    [<CompiledName("TryFilter"); EditorBrowsable(EditorBrowsableState.Never)>]
    let tryFilter = suchThatOption

    /// Generates a list of random length. The maximum length depends on the
    /// size parameter.
    //[category: Creating generators from generators]
    [<CompiledName("ListOf")>]
    let listOf gn =
        sized <| fun n ->
            gen {   let! k = choose (0,n+1) //decrease chance of empty list
                    return! listOfLength k gn }

    /// Generates a non-empty list of random length. The maximum length 
    /// depends on the size parameter.
    //[category: Creating generators from generators]
    [<CompiledName("NonEmptyListOf")>]
    let nonEmptyListOf gn =
        sized <| fun n ->
            gen {   let! k = choose (1,max 1 n)
                    return! listOfLength k gn }

    /// Generates sublists of the given sequence.
    //[category: Creating generators]
    [<CompiledName("SubListOfToList"); EditorBrowsable(EditorBrowsableState.Never)>]
    let subListOf l =
        let elems = Array.ofSeq l
        gen {// Generate indices into the array (up to the number of elements)
             let! size = choose(0, elems.Length-1)
             let! indices = listOfLength size (choose(0, elems.Length-1)) 
             let subSeq = indices |> Seq.distinct |> Seq.map (fun i -> elems.[i])
             return List.ofSeq subSeq }

    /// Generates sublists of the given IEnumerable.
    //[category: Creating generators]
    [<CompiledName("SubListOf"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let subListOfToIList s = 
        subListOf s
        |> map (fun l -> new List<_>(l) :> IList<_>)

    /// Generates sublists of the given arguments.
    //[category: Creating generators]
    [<CompiledName("SubListOf"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let subListOfArr ([<ParamArrayAttribute>] s:_ array) = 
        subListOf s
        |> map (fun l -> new List<_>(l) :> IList<_>)

    /// Generates an array of a specified length.
    //[category: Creating generators from generators]
    [<CompiledName("ArrayOf")>]
    let arrayOfLength n (g: Gen<'a>) : Gen<'a[]> = listOfLength n g |> map Array.ofList

    /// Generates an array using the specified generator. The maximum length is the size+1.
    //[category: Creating generators from generators]
    [<CompiledName("ArrayOf")>]
    let arrayOf (g: Gen<'a>) : Gen<'a[]> = 
       sized <| fun n ->
             gen { let! size = choose(0, n+1) //deccrease chance of empty arrays somewhat
                   return! arrayOfLength size g }

    /// Generates a 2D array of the given dimensions.
    //[category: Creating generators from generators]
    [<CompiledName("Array2DOf")>]
    let array2DOfDim (rows: int,cols: int) (g: Gen<'a>)  = 
        gen { let! arr1 = arrayOfLength (rows * cols) g
              return Array2D.init rows cols (fun r c -> arr1.[cols*r + c]) }

    /// Generates a 2D array. The square root of the size is the maximum number of rows and columns.
    //[category: Creating generators from generators]
    [<CompiledName("Array2DOf")>]
    let array2DOf (g: Gen<'a>) = 
        sized <| fun n ->
            gen { let chooseSqrtOfSize = choose(0, n |> float |> sqrt |> int)
                  let! rows = chooseSqrtOfSize 
                  let! cols = chooseSqrtOfSize
                  return! array2DOfDim (rows,cols) g }
        
    ///Always generate the same instance v. See also fresh.
    //[category: Creating generators]
    [<CompiledName("Constant")>]
    let constant v = gen.Return v

    ///Generate a fresh instance every time the generator is called. Useful for mutable objects.
    ///See also constant.
    //[category: Creating generators]
    [<CompiledName("Fresh"); EditorBrowsable(EditorBrowsableState.Never)>]
    let fresh fv = gen { let a = fv() in return a }

    ///Generate a fresh instance every time the generatoris called. Useful for mutable objects.
    ///See also constant.
    //[category: Creating generators]
    [<CompiledName("Fresh"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false) >]
    let freshFunc (fv:Func<_>) = fresh fv.Invoke

    ///Apply the given Gen function to the given generator, aka the applicative <*> operator.
    //[category: Creating generators from generators]
    [<CompiledName("Apply")>]
    let apply (f:Gen<'a -> 'b>) (gn:Gen<'a>) : Gen<'b> =
        gen { let! f' = f
              let! gn' = gn
              return f' gn' }

    ///Promote the given function f to a function generator. Only used for generating arbitrary functions.
    let internal promote f = Gen (fun n r -> fun a -> let (Gen m) = f a in m n r)

    ///Basic co-arbitrary generator transformer, which is dependent on an int.
    ///Only used for generating arbitrary functions.
    let internal variant<'a,'b when 'a:equality>  =
        let counter = ref 1
        let toCounter = new Dictionary<'a,int>()
        let mapToInt (value:'a) =
            if (box value) = null then 0
            else
                let (found,result) = toCounter.TryGetValue value
                if found then 
                    result
                else
                    toCounter.Add(value,!counter)
                    counter := !counter + 1
                    !counter - 1
        let rec rands r0 = seq { let r1,r2 = split r0 in yield r1; yield! (rands r2) }
        fun (v:'a) (Gen m:Gen<'b>) -> Gen (fun n r -> m n (Seq.nth ((mapToInt v)+1) (rands r)))

///Operators for Gen.
type Gen with

    /// Lifted function application = apply f to a, all in the Gen applicative functor.
    static member (<*>) (f, a) = Gen.apply f a

    /// Like <*>, but puts f in a Gen first.
    static member (<!>) (f, a) = Gen.constant f <*> a

    /// Bind operator; runs the first generator, then feeds the result
    /// to the second generator function.
    static member (>>=) (m,k) = gen.Bind(m,k)
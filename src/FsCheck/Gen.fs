namespace FsCheck

// Disable warnings about calling certain FsCheck functions from F#.
// Using them internally within FsCheck is important for performance reasons.
#nowarn "10001"

// recursive references
#nowarn "40"

type Size = int

/// The state of the GenStream computation.
type Context<'T> = { 
    Next: 'T -> unit
    Reject: unit -> unit
    Size: Size ref
    Seed: Rnd ref
    Shrinks: ShrinkStream<'T> -> unit
}

(*
Generator used to be an interface.
But I had to write
abstract Generate: (unit -> unit)
mind the parens. This makes this into a property that returns a thunk
The thunk is the important bit, not the property. 
If we'd write the more usual:
abstract Generate: unit -> unit
we are much more prone to stack overflows. For some reason, the
F# compiler does not recognize that our calls are in tail position
if we do the latter.

But if we have to do that, we might as well make Generator into
a record type, so we don't have to worry about the thunk
being re-created every time we call Generate.
*)

type Generator = {  
    /// Each time Generate is called, a new random element is generated
    /// and either Next or Reject is called on the context.
    Generate: unit -> unit
    /// Create a new ShrinkStream from the last
    /// generated element and pass it to Shrinks on the context.
    GetShrinks: unit -> unit
}

[<Interface>]
type internal IGen =
    abstract AsGenObject : Gen<obj>

///Generator and shrinker values T.
and Gen<'T> = Gen of (Context<'T> -> Generator) with
    /// Map the given function to the value in the generator, yielding a new generator of the result type.
    member internal src.Map<'U> (f: 'T -> 'U) : Gen<'U> =
        fun { Next=next; Shrinks=shrinks; Reject=reject; Size=size; Seed=seed } ->
            let (Gen src) = src
            src { Next    = fun a -> next (f a)
                  Shrinks = fun str -> shrinks (Shrink.map f str)
                  Reject=reject; Size=size; Seed=seed }
        |> Gen

    interface IGen with
        member x.AsGenObject = x.Map box

/// Computation expression builder for Gen.
[<AutoOpen>]
module GenBuilder =

    let inline private result t =
        fun ctx ->
            { Generate = fun () -> ctx.Next t
              GetShrinks = fun () -> ctx.Shrinks <| Shrink.empty
            }
        |> Gen

    let inline withNext ctx next shrink : Context<'T> =
        { Next = next
          Reject = ctx.Reject
          Size = ctx.Size
          Seed = ctx.Seed
          Shrinks = shrink
        }

    //let inline withNextReject ctx next reject : Context<'T> =
    //    { Next = next
    //      Reject = reject
    //      Size = ctx.Size
    //      Seed = ctx.Seed
    //    }

    let inline internal apply (Gen f) (Gen a) = 
        fun ctx ->
            let mutable lastF = Unchecked.defaultof<'T -> 'U>
            let mutable lastA = Unchecked.defaultof<'T>
            let mutable fShrinkStream = Unchecked.defaultof<ShrinkStream<'T->'U>>
            let aCtx = withNext ctx (fun a -> lastA <- a
                                              ctx.Next (lastF lastA))
                                    (fun s -> ctx.Shrinks <| Shrink.apply lastF lastA fShrinkStream s)
            // genA is lazy here because otherwise the recursive call into a (by passing it aCtx) consumes
            // a stackframe, as it is not in tail position. The call to f is fine because it is in tail 
            // position.
            let genA = lazy a aCtx
            f (withNext ctx (fun f -> lastF <- f
                                      genA.Value.Generate())
                            (fun s -> fShrinkStream <- s
                                      genA.Value.GetShrinks()))
        |> Gen   

    let inline internal shrinks genCtx (Gen str:Gen<'T>) : ShrinkStream<'T> =
        fun ctx ->
            let mutable generatedOne = false
            let mutable shrinkStream = Unchecked.defaultof<Shrinker>
            let rec strCtx = { genCtx with Next = ctx.Next
                                           Reject = fun () -> (str strCtx).Generate() // potential infinite or very long loop
                                           Shrinks = fun s -> shrinkStream <- s ctx }
            { Shrink = fun () ->
                    if not generatedOne then
                        let gen = str strCtx
                        gen.Generate()
                        gen.GetShrinks()
                    else
                        shrinkStream.Shrink()
              GetShrinks = fun () ->
                    shrinkStream.GetShrinks()
            }

    let inline internal bind ((Gen m) : Gen<_>) (k : _ -> Gen<_>) : Gen<_> = 
         fun ctx ->
            let mutable lastStreamU = Unchecked.defaultof<Generator>
            let mutable lastU = Unchecked.defaultof<'U>
            let mutable lastT = Unchecked.defaultof<'T>
            let mutable mShrinkStream = Unchecked.defaultof<ShrinkStream<'T>>

            let uCtx = withNext ctx (fun a -> lastU <- a; ctx.Next lastU) ctx.Shrinks
            let inline setLastStreamU() =
                let (Gen g) = k lastT
                lastStreamU <- g uCtx
            let m = m (withNext ctx (fun t -> lastT <- t; setLastStreamU(); lastStreamU.Generate())
                                    (fun s -> mShrinkStream <- s
                                              let sstr = s |> Shrink.map (k >> shrinks ctx)
                                              ctx.Shrinks <| Shrink.join sstr))
            m
        |> Gen

    let inline private delay (f : unit -> Gen<'T>) : Gen<'T> = 
        // f can have side effects so must be executed every time.
        fun ctx ->
            let inline run() =
                let (Gen g) = f ()
                g ctx
            { Generate = fun () ->
                    let forced = run()
                    forced.Generate()
              GetShrinks = fun () ->
                    let forced = run()
                    forced.GetShrinks()
            }
        |> Gen

    let inline private doWhile p (Gen gen) =
        fun ctx ->
            let ignoreNext = withNext ctx (fun _ -> ()) (fun _ -> ()) |> gen
            { Generate = fun () ->
                    while p() do ignoreNext.Generate()
                    ctx.Next ()
              GetShrinks = fun () ->
                    ctx.Shrinks Shrink.empty
            }
        |> Gen

    //let inline private tryFinally (Gen m) handler = 
    //    Gen (fun n r -> try m n r finally handler ())

    let inline private dispose (x: #System.IDisposable) = x.Dispose()

//    let inline private using r f = tryFinally (f r) (fun () -> dispose r)

    ///The workflow type for generators.
    type GenBuilder internal() =
        member __.Return(a) : Gen<_> = result a
        member __.Bind(m, k) : Gen<_> = bind m k
        member __.Delay(f) : Gen<_> = delay f
        member __.Combine(m1, m2) = bind m1 (fun () -> m2)
//        member __.TryFinally(m, handler) = tryFinally m handler
//        member __.TryWith(Gen m, handler) = Gen (fun n r -> try m n r with e -> GeneratedValue(handler e,r))
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

///Combinators to build custom random generators for any type.
module Gen =

    open Common
    open System
    open System.Collections.Generic
    open System.ComponentModel

    ///Always generate the same instance v. See also fresh.
    //[category: Creating generators]
    [<CompiledName("Constant")>]
    let constant v = gen.Return v

    ///Rejects every value.
    //[category: Creating generators]
    let reject<'T> : Gen<'T> =
        fun ctx -> 
            { Generate = fun () -> ctx.Reject ()
              GetShrinks = fun () -> ctx.Shrinks <| Shrink.empty
            }
        |> Gen

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

    ///Apply the function f to the value in the generator, yielding a new generator.
    //[category: Creating generators from generators]
    [<CompiledName("Map"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map f (gen:Gen<_>) = gen.Map f

    ///Obtain the current size. sized g calls g, passing it the current size as a parameter.
    //[category: Managing size]
    [<CompiledName("Sized"); EditorBrowsable(EditorBrowsableState.Never)>]
    let sized sfun = Gen (fun ctx -> let (Gen g) = sfun !ctx.Size in g ctx)

    ///Obtain the current size. sized g calls g, passing it the current size as a parameter.
    //[category: Managing size]
    [<CompiledName("Sized"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let sizedFunc (sizedGen : Func<int,Gen<_>>) =
        sized sizedGen.Invoke

    ///Override the current size of the test. resize n g invokes generator g with size parameter n.
    //[category: Managing size]
    [<CompiledName("Resize")>]
    let resize size (Gen gen) =
        fun ctx -> 
            let original = !ctx.Size
            try
                ctx.Size := size
                gen ctx
            finally
                ctx.Size := original
        |> Gen

    let private scanResults seed size ((Gen gen):Gen<'T>) =
        let result = ref Unchecked.defaultof<'T>
        let rejected = ref 0
        let mutable haveResult = false
        let one =  gen { Next = fun r -> haveResult <- true; result := r
                         Reject = fun () -> rejected := !rejected + 1;
                         Shrinks = fun s -> () //not shrinking here at the moment
                         Size = ref size
                         Seed = ref seed }
        let next() =
            while not haveResult do one.Generate()
            haveResult <- false
        next, result, rejected

    ///Generates n values of the given size and starting with the given seed.
    //[category: Generating test values]
    [<CompiledName("Sample")>]
    let sampleWithSeed seed size nbSamples (gen:Gen<'T>) : 'T[] =
        let next, result, _ = scanResults seed size gen
        [| for i in 0..nbSamples-1 do 
             next()
             yield !result |]

    ///Generates a given number of values with a new seed and a given size.
    //[category: Generating test values]
    [<CompiledName("Sample")>]
    let sampleWithSize size nbSamples gen : 'T[]= sampleWithSeed (Random.create()) size nbSamples gen

    ///Generates a given number of values with a new seed and a size of 50.
    //[category: Generating test values]
    [<CompiledName("Sample")>]
    let sample nbSamples gen : 'T[] = sampleWithSize 50 nbSamples gen

    let inline internal primitiveStream generate shrink = 
        fun ctx -> 
            let mutable current = Unchecked.defaultof<'a> 
            let generate = generate ctx
            { Generate = fun () ->
                current <- generate ()
                ctx.Next current
              GetShrinks = fun () -> 
                ctx.Shrinks <| Shrink.shrink current shrink
            }
        |> Gen

    /// Generates a random int64 uniformly distributed between lo and hi (both inclusive),
    /// and shrinks towards target.
    //[category: Creating generators]
    let choose64Around target (lo,hi) =
        let (lo,hi) = if hi < lo then (hi,lo) else (lo,hi)

        let generate ctx =
            fun () -> Random.rangeInt64(lo, hi, !ctx.Seed, ctx.Seed)

        let shrink current =
            seq { if current <> target then 
                        yield target
                  let mutable c = current
                  while abs (c - target) > 1L do
                        c <- c - (c - target) / 2L
                        yield c }

        primitiveStream generate shrink

    let inline internal withShrinkStream (str:'T-> ShrinkStream<'T>) (Gen genF:Gen<'T>) : Gen<'T> =
        fun ctx ->  
            let mutable current = Unchecked.defaultof<'T>
            let gen = genF (withNext ctx (fun e -> current <- e; ctx.Next e) (fun shr -> ()))
            { Generate = fun () ->
                gen.Generate()
              GetShrinks = fun () ->
                ctx.Shrinks (str current)
            }
        |> Gen

    let shrink (shrinker:'T -> seq<'T>) (gen:Gen<'T>) : Gen<'T> =
        withShrinkStream (fun cur -> Shrink.shrink cur shrinker) gen

    /// Generates a random int64 uniformly distributed between lo and hi (both inclusive),
    /// and shrinks towards lo.
    //[category: Creating generators]
    let choose64 (lo,hi) = choose64Around lo (lo,hi)

    /// Generates a random int uniformly distributed between lo and hi (both inclusive),
    /// and shrinks towards lo.
    //[category: Creating generators]
    let chooseAround target (lo,hi) = choose64Around (int64 target) (int64 lo, int64 hi) |> map int

    ///Generates an integer between l and h, inclusive. Shrink to lo.
    //[category: Creating generators]
    [<CompiledName("Choose")>]
    let choose (lo,hi) = chooseAround lo (lo,hi)

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    //[category: Creating generators]
    [<CompiledName("Elements"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let elementsArr ([<ParamArrayAttribute>] values : array<_>) =
        // Similar to `elements` but specialized to arrays for performance reasons.
        choose (0, values.Length - 1) |> map (Array.get values)

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    //[category: Creating generators]
    [<CompiledName("Elements")>]
    let elements (xs : seq<_>) =
        // If the sequence is an array, use the implementation specialized for arrays.
        match xs with
        | :? array<_> as arr ->
            elementsArr arr
        | _ ->
            choose (0, (Seq.length xs)-1) |> map (flip Seq.item xs)

    ///Build a generator that takes a non-empty sequence and randomly generates
    ///one of the values among an initial segment of that sequence. The size of
    ///this initial segment increases with the size parameter. Essentially this
    ///generator is Gen.elements but taking also the runtime size into account.
    //[category: Creating generators]
    [<CompiledName("GrowingElements")>]
    let growingElements xs =
        let arr = Seq.toArray xs
        sized (fun s ->
            let s' = max 1 s
            let n  = min arr.Length s'
            elements (arr |> Seq.take n))

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///equal probability.
    //[category: Creating generators from generators]
    [<CompiledName("OneOf")>]
    let oneof gens = bind (elements gens) id

    ///Build a generator that generates a value from one of the given generators, with
    ///equal probability.
    //[category: Creating generators from generators]
    [<CompiledName("OneOf"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let oneOfArr ([<ParamArrayAttribute>]  generators : array<Gen<_>>) = 
        // Effectively the same as:
        //  generators |> oneof
        // but specialized to arrays for performance.
        gen.Bind(elementsArr generators, id)

    /// <summary>
    /// Build a generator that generates a value from one of the generators in the given non-empty seq, with
    /// given probabilities. The sum of the probabilities must be larger than zero.
    /// </summary>
    /// <param name="xs">Sequence of tuples where each tuple contains a weight and a generator.</param>
    /// <exception cref="System.ArgumentException">Thrown if the sum of the probabilities is less than or equal to 0.</exception>
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
            bind (choose (1,tot)) (pick 0)

    /// <summary>
    /// Build a generator that generates a value from one of the generators in the given non-empty seq, with
    /// given probabilities. The sum of the probabilities must be larger than zero.
    /// </summary>
    /// <param name="weightedValues">Array of tuples where each tuple contains a weight and a generator.</param>
    /// <exception cref="System.ArgumentException">Thrown if the sum of the probabilities is less than or equal to 0.</exception>
    //[category: Creating generators from generators]
    [<CompiledName("Frequency"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let frequencyTupleArr ( [<ParamArrayAttribute>] weightedValues : (int * Gen<'a>)[] ) =
        weightedValues |> frequency

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map2"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map2 f a b = apply (map f a) b
    
    ///Build a generator that generates a 2-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Two")>]
    let two g = map2 (fun a b -> (a,b)) g g

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map3"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map3 f a b c = apply (apply (map f a) b) c

    ///Build a generator that generates a 3-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Three")>]
    let three g = map3 (fun a b c -> (a,b,c)) g g g

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map4"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map4 f a b c d = apply (apply (apply (map f a) b) c) d

    ///Build a generator that generates a 4-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Four")>]
    let four g = map4 (fun a b c d -> (a,b,c,d)) g g g g

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map5"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map5 f a b c d e = apply (apply (apply (apply (map f a) b) c) d) e

    ///Map the given function over values to a function over generators of those values.
    //[category: Creating generators from generators]
    [<CompiledName("Map6"); EditorBrowsable(EditorBrowsableState.Never)>]
    let map6 f a b c d e g = apply (apply (apply (apply (apply (map f a) b) c) d) e) g

    ///Combine two generators into a generator of pairs.
    //[category: Creating generators from generators]
    let zip f g = map2 (fun x y -> x, y) f g

    ///Combine three generators into a generator of 3-tuples.
    //[category: Creating generators from generators]
    let zip3 f g h = map3 (fun x y z -> x, y, z) f g h

    ///Split a generator of pairs into a pair of generators.
    //[category: Creating generators from generators]
    let unzip g = map fst g, map snd g

    ///Split a generator of 3-tuples into a 3-tuple of generators.
    //[category: Creating generators from generators]
    let unzip3 g =
        map (fun (x, _, _) -> x) g,
        map (fun (_, y, _) -> y) g,
        map (fun (_, _, z) -> z) g

    let inline sequenceToArrImpl (generators:Gen<'T>[]) shrinker : Gen<'T[]> =
        fun ctx ->
            let values = Array.zeroCreate generators.Length
            let shrinkers = Array.zeroCreate<ShrinkStream<'T>> generators.Length
            let mutable index = 0
            let gCtx = withNext ctx (fun st -> values.[index] <- st)
                                    (fun shr -> shrinkers.[index] <- shr)
            let gensGen = generators |> Array.map (fun (Gen g) -> g gCtx)

            { Generate = fun () -> 
                for i in 0..values.Length-1 do
                    index <- i
                    gensGen.[i].Generate()
                ctx.Next (Array.copy values)
              GetShrinks = fun () -> 
                for i in 0..values.Length-1 do
                    index <- i
                    gensGen.[i].GetShrinks()
                ctx.Shrinks <| shrinker (Array.copy values) shrinkers }
        |> Gen


    /// Sequence the given array of generators into a generator of a array.
    //[category: Creating generators from generators]
    [<CompiledName("Sequence")>]
    let sequenceToArr ([<ParamArrayAttribute>]generators:Gen<'T>[]) : Gen<'T[]> =
        if Object.ReferenceEquals (null, generators) then
            nullArg "generators"

        sequenceToArrImpl generators Shrink.elements

    /// Sequence the given enumerable of generators into a generator of a list.
    //[category: Creating generators from generators]
    [<CompiledName("SequenceToList"); EditorBrowsable(EditorBrowsableState.Never)>]
    let sequence (gens:Gen<'T> seq) : Gen<'T list> =
        gens |> Seq.toArray |> sequenceToArr |> map List.ofArray 

    /// Sequence the given enumerable of generators into a generator of an enumerable.
    //[category: Creating generators from generators]
    [<CompiledName("Sequence")>]
    let sequenceToSeq generators = 
        if Object.ReferenceEquals (null, generators) then
            nullArg "generators"

        generators |> Seq.toArray |> sequenceToArr |> map (fun a -> a :> seq<_>)

    ///Generates a list of given length, containing values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("ListOf")>]
    let listOfLength n arb = sequence [ for _ in 1..n -> arb ]

    let inline private shuffleInPlace (arr: _ array) =
        // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
        let inline swap (arr : _ array) i j =
            let v = arr.[j]
            arr.[j] <- arr.[i]
            arr.[i] <- v
        gen {
            let maxI = arr.Length - 1
            let! indexes =
                [| for i in 0..maxI - 1 -> choose (i, maxI) |]
                |> sequenceToArr
            indexes |> Array.iteri (swap arr)
            return arr
        }

    ///Generates a random permutation of the given sequence.
    //[category: Creating generators]
    [<CompiledName("Shuffle")>]
    let shuffle xs =
        let xs = xs |> Seq.toArray
        gen {
            let copy = Array.copy xs
            return! shuffleInPlace copy
        }

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values by increasing its size.
    ///
    ///The `tryWhere` function is also aliased as `tryFilter`. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("TryWhere"); EditorBrowsable(EditorBrowsableState.Never)>]
    let tryWhere predicate generator = 
        let rec tryValue k n =
            match (k,n) with 
            | (_,0) -> gen { return None }
            | (k,n) -> gen { let! x = resize (2*k+n) generator
                             if predicate x then return Some x else return! tryValue (k+1) (n-1) }
        sized (tryValue 0 << max 1)

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values by increasing its size.
    ///
    ///The `tryFilter` function is an alias for the `tryWhere` function. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("TryFilter"); EditorBrowsable(EditorBrowsableState.Never)>]
    let tryFilter predicate generator = tryWhere predicate generator

    ///Generates a value that satisfies a predicate. Contrary to tryWhere, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high probability that 
    ///the predicate is satisfied.
    ///
    ///The `where` function is also aliased as `filter`. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("Where");EditorBrowsable(EditorBrowsableState.Never)>]
    let rec where predicate generator = 
        gen { let! mx = tryWhere predicate generator
              match mx with
              | Some x    -> return x
              | None      -> return! sized (fun n -> resize (n+1) (where predicate generator)) }
    
    ///Generates a value that satisfies a predicate. Contrary to tryFilter, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high probability that 
    ///the predicate is satisfied.
    ///
    ///The `filter` function is an alias for the `where` function. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("Filter");EditorBrowsable(EditorBrowsableState.Never)>]
    let filter predicate generator = where predicate generator

    /// Generates a random array of length k where the sum of
    /// all elements equals the given sum.
    //[category: Creating generators]
    [<CompiledName("Piles")>]
    let piles k sum = 
        let genSorted p n m =
            gen {
                let result = Array.zeroCreate<int> k
                let mutable n = n
                let mutable m = m
                for i in p..(-1)..1 do
                    if i = 1 then
                        result.[i-1] <- n
                    else
                        let! r = choose (int (ceil(float n / float i)), min m n)
                        result.[i-1] <- r
                        n <- n-r
                        m <- min m r
                return result
            }
        if k <= 0 then
            constant [||]
        else
            bind (genSorted k sum sum) shuffleInPlace

    /// Generates an array of a specified length.
    //[category: Creating generators from generators]
    [<CompiledName("ArrayOf")>]
    let arrayOfLength n (g: Gen<'a>) : Gen<'a[]> =
        // For compatibility with the way `listOfLength` is implemented,
        // if the length is negative we just return an empty array.
        if n < 1 then Array.empty else Array.create n g
        |> sequenceToArr

    /// Generates an array using the specified generator. The maximum length is the size+1.
    //[category: Creating generators from generators]
    [<CompiledName("ArrayOf")>]
    let arrayOf (g: Gen<'a>) : Gen<'a[]> = 
       sized <| fun n ->
             gen { let! k = choose(0, n)
                   let! sizes = piles k n
                   return! sequenceToArrImpl [| for size in sizes -> resize size g |] Shrink.arrayThenElements }
  
    /// Generates a list of random length. The maximum length depends on the
    /// size parameter.
    //[category: Creating generators from generators]
    [<CompiledName("ListOf")>]
    let listOf gn =
        arrayOf gn |> map Array.toList

    /// Generates a non-empty list of random length. The maximum length 
    /// depends on the size parameter.
    //[category: Creating generators from generators]
    [<CompiledName("NonEmptyListOf")>]
    let nonEmptyListOf gn =
        sized <| fun n ->
            gen { let! k = choose (1,max 1 n)
                  let! sizes = piles k n
                  return! sequence [ for size in sizes -> resize size gn ] }

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

    ///Generate an option value that is 'None' 1/8 of the time.
    //[category: Creating generators from generators]
    let optionOf g = frequency [(1, constant None); (7, map Some g)]

    ///Apply the given Gen function to the given generator, aka the applicative <*> operator.
    //[category: Creating generators from generators]
    [<CompiledName("Apply")>]
    let apply (f:Gen<'T->'U>) (gn:Gen<'T>) : Gen<'U> = apply f gn

    /// Generate an F# function, of type FSharpFunc. The functions are pure,
    /// i.e. if called with the same input, the same output value is produced.
    let pureFunction<'T,'U when 'T:equality> (Gen u:Gen<'U>) : Gen<'T->'U> = 
        let getOrAdd (map:IDictionary<_,_>) (value:'T) create =
            if isNull (box value) then Unchecked.defaultof<'U>
            else
                lock map (fun _ ->
                    let (found,result) = map.TryGetValue value
                    if found then 
                        result
                    else
                        let result = create()
                        map.Add(value,result)
                        result)

        fun ctx ->
            { Generate = fun () ->
                    let mutable nextU = Unchecked.defaultof<'U>
                    let fCtx = { Next = fun u -> nextU <- u
                                 Reject = id // what about rejects...?
                                 Shrinks = fun _ -> () // can we shrink?
                                 Size = ref !ctx.Size
                                 Seed = ref <| Rnd() }
                    Random.split (!ctx.Seed, ctx.Seed, fCtx.Seed)

                    let map = new Dictionary<'T,'U>()
                    let fGen = u fCtx
                    ctx.Next (fun a -> getOrAdd map a (fun () -> fGen.Generate(); nextU))
              GetShrinks = fun () -> ()
            }
        |> Gen

///Operators for Gen.
type Gen<'T> with

    /// Apply f to a.
    static member (<*>) (f, a) = apply f a

    /// Map f over a.
    static member (<!>) (f, a) = Gen.constant f <*> a

    /// Runs the first generator, then feeds the result
    /// to the second generator function.
    static member (>>=) (m,k) = bind m k
namespace FsCheck

// this interface is necessary for reflection - hopefullyl with TypeShape
// we can get rid of it.
[<Interface>]
type internal IGen =
    abstract AsGenObject : Gen<obj>

/// Generator of a random value, based on a size parameter and a randomly generated int.
and [<NoEquality;NoComparison>] Gen<'T> = private Gen of (int -> Rnd -> struct ('T * Rnd)) with
    interface IGen with
        member t.AsGenObject = 
            let (Gen g) = t
            Gen (fun s r0 -> let struct (value,r1) = g s r0 in struct (box value, r1))


namespace FsCheck.FSharp

open FsCheck

[<RequireQualifiedAccess>]
module Gen =

    open System
    open System.Collections.Generic

    let inline internal run size rnd (Gen gen) =
        let struct(v,_) = gen size rnd
        v

    /// Creates a generator by allowing a create function to get values out
    /// of a generator
    let inline internal promote create :Gen<'U> =
        Gen (fun s r -> let mr = create (run s r) in struct(mr,r))
        //Gen (fun s r -> let mr = Rose.map (Gen.run s r) m in struct (mr,r))

    /// Always generate the same given value. See also fresh.
    //[category: Creating generators]
    [<CompiledName("Constant")>]
    let constant (value:'T) = Gen (fun _ seed -> struct (value,seed))

    /// Create a new generator by applying f to each value in the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Map")>]
    let map (f:'T->'U) (Gen g:Gen<'T>) = 
        Gen (fun s r0 ->
                let struct (value,r1) = g s r0
                struct (f value, r1))

    /// Generate fresh instances by calling create every time the generator 
    /// generates a new value. Useful for generating new instances of mutable
    /// objects.
    /// See also constant.
    //[category: Creating generators]
    [<CompiledName("Fresh")>]
    let fresh create :Gen<'T> = 
        constant () |> map create

    /// Apply the functions f from the first generator to the values from the second generator pairwise,
    /// yielding a new generator that generates the results.
    //[category: Creating generators from generators]
    [<CompiledName("Apply")>]
    let apply (Gen f:Gen<'T->'U>) (Gen a:Gen<'T>) :Gen<'U> = 
        Gen (fun n r0 -> let struct (f',r1) = f n r0
                         let struct (a',r2) = a n r1
                         struct (f' a', r2))

    /// Creates a new generator that generates values from the source generator, 
    /// applies the function k to them, and generates values from the resulting generator.
    //[category: Creating generators from generators]
    [<CompiledName("Bind")>]
    let bind (k : _ -> Gen<'U>) (Gen g : Gen<'T>) : Gen<_> = 
        Gen (fun n r0 -> let struct(v,r1) = g n r0
                         let (Gen g') = k v
                         g' n r1)

    /// Create a new generator by applying f to each value in the given generators.
    //[category: Creating generators from generators]
    [<CompiledName("Map")>]
    let map2 f (Gen a) (Gen b) = 
        Gen (fun size r0 ->
                let struct (a',r1) = a size r0
                let struct (b',r2) = b size r1
                struct (f a' b', r2))

    /// Create a new generator by applying f to each value in the given generators.
    //[category: Creating generators from generators]
    [<CompiledName("Map")>]
    let map3 f (Gen a) (Gen b) (Gen c) = 
        Gen (fun size r0 ->
                let struct (a',r1) = a size r0
                let struct (b',r2) = b size r1
                let struct (c',r3) = c size r2
                struct (f a' b' c', r3))

    /// Create a new generator by applying f to each value in the given generators.
    //[category: Creating generators from generators]
    [<CompiledName("Map")>]
    let map4 f (Gen a) (Gen b) (Gen c) (Gen d) = 
        Gen (fun size r0 ->
                let struct (a',r1) = a size r0
                let struct (b',r2) = b size r1
                let struct (c',r3) = c size r2
                let struct (d',r4) = d size r3
                struct (f a' b' c' d', r4))

    /// Create a new generator by applying f to each value in the given generators.
    //[category: Creating generators from generators]
    [<CompiledName("Map")>]
    let map5 f (Gen a) (Gen b) (Gen c) (Gen d) (Gen e) = 
        Gen (fun size r0 ->
                let struct (a',r1) = a size r0
                let struct (b',r2) = b size r1
                let struct (c',r3) = c size r2
                let struct (d',r4) = d size r3
                let struct (e',r5) = e size r4
                struct (f a' b' c' d' e', r5))

        /// Create a new generator by applying f to each value in the given generators.
    //[category: Creating generators from generators]
    [<CompiledName("Map")>]
    let map6 f (Gen a) (Gen b) (Gen c) (Gen d) (Gen e) (Gen g) = 
        Gen (fun size r0 ->
                let struct (a',r1) = a size r0
                let struct (b',r2) = b size r1
                let struct (c',r3) = c size r2
                let struct (d',r4) = d size r3
                let struct (e',r5) = e size r4
                let struct (g',r6) = g size r5
                struct (f a' b' c' d' e' g', r6))
        
    /// Obtain the current size. sized g calls g, passing it the current size as a parameter.
    //[category: Managing size]
    [<CompiledName("Sized")>]
    let sized gen : Gen<'T> = Gen (fun n r -> let (Gen m) = gen n in m n r)

    /// Override the current size of the test. resize n g invokes generator g with size parameter n.
    //[category: Managing size]
    [<CompiledName("Resize")>]
    let resize newSize (Gen m) : Gen<'T> = Gen (fun _ r -> m newSize r)

    /// Modify a size using the given function before passing it to the given Gen.
    //[category: Creating generators from generators]
    [<CompiledName("ScaleSize")>]
    let scaleSize f g : Gen<'T> = sized (fun size -> resize (f size) g)

    /// Generates n values of the given size and starting with the given seed.
    //[category: Generating test values]
    [<CompiledName("Sample")>]
    let sampleWithSeed seed size nbSamples (Gen generator) :'T[] = 
        let mutable currentRnd = seed
        [| for i in 1..nbSamples do
            let struct (sv,rnd') = generator size currentRnd
            currentRnd <- rnd'
            yield sv |]

    /// Generates a given number of values with a new seed and a given size.
    //[category: Generating test values]
    [<CompiledName("Sample")>]
    let sampleWithSize size nbSamples gen : 'T[] = sampleWithSeed (Random.Create()) size nbSamples gen

    /// Generates a given number of values with a new seed and a size of 50.
    //[category: Generating test values]
    [<CompiledName("Sample")>]
    let sample nbSamples gen : 'T[] = sampleWithSize 50 nbSamples gen

    /// Generates ints between l and h, inclusive.
    //[category: Creating generators]
    [<CompiledName("Choose")>]
    let choose (l,h) = Gen (fun _ r ->
        let x, r = Random.RangeInt (l,h,r)
        struct (x, r))

    /// Generates int64 between l and h, inclusive.
    //[category: Creating generators]
    [<CompiledName("Choose64")>]
    let choose64 (l,h) = Gen (fun _ r ->
        let x, r = Random.RangeInt64 (l,h,r)
        struct (x, r))

    /// Generates double values in [0, 1).
    let internal double =
        Gen (fun _ r ->
            let mutable r1 = r
            let x = FsCheck.Random.NextDouble(&r1)
            struct (x, r1)
        )

    [<Struct>]
    type internal ListAccessWrapper<'T> =
        { Count: int
          Item: int -> 'T
        }
        static member Create(xs:seq<_>) =
            match xs with
            | :? array<_> as arr ->
                { Count=arr.Length; Item=fun i -> arr.[i] }
            | :? IReadOnlyList<_> as list ->
                { Count=list.Count; Item=fun i -> list.[i] }
            | :? IList<_> as list ->
                { Count=list.Count; Item=fun i -> list.[i] }
            | _ ->
                let arr = xs |> Seq.toArray
                { Count=arr.Length; Item=fun i -> arr.[i] }

    ///Build a generator that randomly generates one of the values in the given non-empty, finite seq.
    //[category: Creating generators]
    [<CompiledName("Elements")>]
    let elements (xs : seq<'T>) =
        let elems = ListAccessWrapper<_>.Create xs
        choose (0, elems.Count-1) |> map elems.Item
    
    ///Build a generator that takes a non-empty sequence and randomly generates
    ///one of the values among an initial segment of that sequence. The size of
    ///this initial segment increases with the size parameter. Essentially this
    ///generator is Gen.elements but taking also the size into account.
    //[category: Creating generators]
    [<CompiledName("GrowingElements")>]
    let growingElements (xs:seq<'T>) =
        let arr = Seq.toArray xs
        sized (fun s ->
            let s' = max 1 s
            let n  = min arr.Length s'
            // ArraySegment is IReaoOnlyList so this
            // will go through a fast path in elements.
            let segment = ArraySegment(arr, 0, n)
            elements segment)

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///equal probability.
    //[category: Creating generators from generators]
    [<CompiledName("OneOf")>]
    let oneof (gens:seq<Gen<'T>>) = bind id (elements gens)

    /// <summary>
    /// Build a generator that generates a value from one of the generators in the given non-empty seq, with
    /// given probabilities. The sum of the probabilities must be larger than zero.
    /// </summary>
    /// <param name="dist">Sequence of tuples where each tuple contains a weight and a generator.</param>
    /// <exception cref="System.ArgumentException">Thrown if the sum of the probabilities is less than or equal to 0.</exception>
    //[category: Creating generators from generators]
    [<CompiledName("Frequency")>]
    let frequency (dist:seq<int*Gen<'T>>) =
        let xs = ListAccessWrapper<_>.Create dist
        let tot = Seq.sumBy fst dist
        let rec pick i n =
            let k,x = xs.[i]
            if n<=k then x else pick (i+1) (n-k)
        if tot <= 0 then 
            invalidArg "dist" "Frequency was called with a sum of probabilities less than or equal to 0. No elements can be generated."
        else
            bind (pick 0) (choose (1,tot)) 

    ///Build a generator that generates a 2-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Two")>]
    let two (g:Gen<'T>) = map2 (fun a b -> (a,b)) g g

    ///Build a generator that generates a 3-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Three")>]
    let three (g:Gen<'T>) = map3 (fun a b c -> (a,b,c)) g g g

    ///Build a generator that generates a 4-tuple of the values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("Four")>]
    let four (g:Gen<'T>) = map4 (fun a b c d -> (a,b,c,d)) g g g g

    ///Combine two generators into a generator of pairs.
    //[category: Creating generators from generators]
    [<CompiledName("Zip")>]
    let zip (f:Gen<'T1>) (g:Gen<'T2>) = map2 (fun x y -> x, y) f g

    ///Combine three generators into a generator of 3-tuples.
    //[category: Creating generators from generators]
    [<CompiledName("Zip")>]
    let zip3 (f:Gen<'T1>) (g:Gen<'T2>) (h:Gen<'T3>) = map3 (fun x y z -> x, y, z) f g h

    /// Traverse the given enumerable into a generator of a list using the specified binder function to create generators.
    ///[category: Create generators from generators]
    [<CompiledName("CollectToList")>]
    let collectToList (f:'T->Gen<'U>) (source:seq<'T>) =
        let l = Seq.map f source |> Seq.toList
        let rec go gs acc size r0 = 
            match gs with
            | [] ->
                struct (List.rev acc, r0)
            | (Gen g)::gs' ->
                let struct (v,r1) = g size r0
                go gs' (v::acc) size r1
        Gen (fun n r -> go l [] n r)

    /// Traverse the given array into a generator of an array using the specified binder function to create generators.
    ///[category: Creating generators from generators]
    [<CompiledName("CollectToArray")>]
    let collectToArray (f:'T->Gen<'U>) (source:seq<'T>) =
        // This implementation is the same as that used for lists and sequences,
        // but is specialized for arrays (to avoid intermediate allocations which
        // would otherwise be caused by conversion to/from list or seq).
        let source = Seq.map f source |> Seq.toArray
        Gen (fun size r0 ->
            let result = Array.zeroCreate source.Length
            let mutable r' = r0
            for i = 0 to source.Length - 1 do
                let (Gen g) = source.[i]
                let struct(v,r1) = g size r'
                r' <- r1
                result.[i] <- v

            struct (result,r'))

    /// Traverse the given enumerable into a generator of an enumerable using the specified binder function to create generators.
    /// Each seq generated by the resulting generator can be infinite, if the source seq is infinite.
    ///[category: Create generators from generators]
    [<CompiledName("CollectToSeq")>]
    let collectToSeq (f:'T->Gen<'U>) (source:seq<'T>) : Gen<seq<_>> =
        // This implementation is similar to that for arrays and lists but is specialized
        // to sequences to avoid intermediate conversion to an intermediate list.
        Gen (fun s r0 ->
                let mutable r1 = Rnd()
                let mutable r2 = Rnd()            
                Random.Split(r0, &r1, &r2)
                let result =
                    seq { let mutable r' = r1
                          for x in source do
                            let (Gen g) = f x
                            let struct (v,r1) = g s r'
                            r' <- r1
                            yield v
                    }
                struct (result, r2))

    /// Sequence the given enumerable of generators into a generator of a list.
    //[category: Creating generators from generators]
    [<CompiledName("SequenceToList")>]
    let sequenceToList (source:seq<Gen<'T>>) =
        collectToList id source

    /// Sequence the given array of generators into a generator of an array.
    //[category: Creating generators from generators]
    [<CompiledName("SequenceToArray")>]
    let sequenceToArray (source:seq<Gen<'T>>) = 
        collectToArray id source

    /// Sequence the given seq of generators into a generator of a seq.
    /// Each seq generated by the resulting generator can be infinite, if the source seq is infinite.
    //[category: Creating generators from generators]
    [<CompiledName("SequenceToSeq")>]
    let sequenceToSeq (source:seq<Gen<'T>>) =
        collectToSeq id source

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values by increasing its size.
    ///
    ///The `tryWhere` function is also aliased as `tryFilter`. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("TryWhere")>]
    let tryWhere (predicate:'T->bool) generator = 
        let rec tryValue k s =
            match (k,s) with 
            | (_,0) -> constant None
            | (k,s) -> (resize (2*k+s) generator) |> bind (fun x -> 
                         if predicate x then constant (Some x)
                         else tryValue (k+1) (s-1))
        sized (max 1 >> tryValue 0)

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values by increasing its size.
    ///
    ///The `tryFilter` function is an alias for the `tryWhere` function. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("TryFilter")>]
    let tryFilter (predicate:'T->bool) generator = tryWhere predicate generator

    ///Generates a value that satisfies a predicate. Contrary to tryWhere, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high probability that 
    ///the predicate is satisfied.
    ///
    ///The `where` function is also aliased as `filter`. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("Where")>]
    let rec where (predicate:'T->bool) generator = 
        tryWhere predicate generator |> bind (fun mx ->
              match mx with
              | Some x    -> constant x
              | None      -> sized (fun n -> resize (n+1) (where predicate generator)))
    
    ///Generates a value that satisfies a predicate. Contrary to tryFilter, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high probability that 
    ///the predicate is satisfied.
    ///
    ///The `filter` function is an alias for the `where` function. These two functions are identical, but co-exist
    ///so that you can choose the word that makes your code most readable in your given context.
    //[category: Creating generators from generators]
    [<CompiledName("Filter")>]
    let filter (predicate:'T->bool) generator = where predicate generator

    let inline private shuffleInPlace (arr: array<_>) =
        // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
        let inline swap (arr : array<_>) i j =
            let v = arr.[j]
            arr.[j] <- arr.[i]
            arr.[i] <- v
        let maxI = arr.Length - 1
        [| for i in 0..maxI - 1 -> choose (i, maxI) |]
        |> sequenceToArray
        |> map (fun indexes -> Array.iteri (swap arr) indexes; arr)

    ///Generates random permutations of the given sequence.
    //[category: Creating generators]
    [<CompiledName("Shuffle")>]
    let shuffle (xs:seq<'T>) =
        let xs = xs |> Seq.toArray
        fresh (fun () -> Array.copy xs)
         |> bind shuffleInPlace

    /// Generates random arrays of given length where the sum of
    /// all elements equals the given sum.
    //[category: Creating generators]
    [<CompiledName("Piles")>]
    let piles length sum = 
        let genSorted p n m = Gen (fun s r0 ->
            let result = Array.zeroCreate<int> p
            let mutable n = n
            let mutable m = m
            let mutable r = r0
            for i in p..(-1)..1 do
                if i = 1 then 
                    result.[i-1] <- n
                else
                    let next = Random.RangeInt(int (ceil(float n / float i)), min m n, r, &r)
                    result.[i-1] <- next
                    n <- n-next
                    m <- min m next
            struct (result,r))
        if length <= 0 then
            constant [||]
        else
            genSorted length sum sum |> bind shuffleInPlace

    /// Generates lists of given length, containing values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("ListOf")>]
    let listOfLength length gen :Gen<list<'T>> = 
        sequenceToList [ for _ in 1..length -> gen ]

    /// Generates lists of random lengths between zero and size.
    //[category: Creating generators from generators]
    [<CompiledName("ListOf")>]
    let listOf gen :Gen<list<'T>> =
        sized (fun n ->
            choose (0,n)
            |> bind (fun k ->
                piles k n
                |> bind (fun sizes -> sequenceToList [ for size in sizes -> resize size gen ])))

    /// Generates non-empty lists of random lengths between one and size.
    //[category: Creating generators from generators]
    [<CompiledName("NonEmptyListOf")>]
    let nonEmptyListOf gen :Gen<list<'T>> =
        sized (fun n ->
            choose (1,max 1 n)
            |> bind (fun k ->
                piles k n
                |> bind (fun sizes -> sequenceToList [ for size in sizes -> resize size gen ])))


    /// Generates arrays of given length, containing values generated by the given generator.
    //[category: Creating generators from generators]
    [<CompiledName("ArrayOf")>]
    let arrayOfLength length gen : Gen<'T[]> =
        sequenceToArray [| for _ in 1..length -> gen |]

    /// Generates arrays of random length between zero and size.
    //[category: Creating generators from generators]
    [<CompiledName("ArrayOf")>]
    let arrayOf gen: Gen<'T[]> = 
       sized (fun n ->
            choose (0,n)
            |> bind (fun k ->
                piles k n
                |> bind (fun sizes -> sequenceToArray [| for size in sizes -> resize size gen |])))

    /// Generates 2D arrays of the given dimensions.
    //[category: Creating generators from generators]
    [<CompiledName("Array2DOf")>]
    let array2DOfDim (rows: int) (cols: int) gen :Gen<'T[,]> = 
        arrayOfLength (rows * cols) gen
        |> map (fun arr -> Array2D.init rows cols (fun r c -> arr.[cols*r + c]))

    /// Generates a 2D array. The square root of the size is the maximum number of rows and columns.
    //[category: Creating generators from generators]
    [<CompiledName("Array2DOf")>]
    let array2DOf gen :Gen<'T[,]> = 
        let chooseSqrtOfSize n = choose(0, n |> float |> sqrt |> int)
        sized (fun n ->
            chooseSqrtOfSize n
            |> two 
            |> bind (fun (rows,cols) -> array2DOfDim rows cols gen))


    // bit out of place, but need it for the next function.
    let internal bool = choose(0,1) |> map ((=) 1)

    /// Generates sublists of the given seq. For a given list of length n,
    /// each sublist has between 0 and n elements, and the order of the 
    /// elements is the same as in the given seq.
    //[category: Creating generators]
    [<CompiledName("SubListOf")>]
    let subListOf lst :Gen<list<'T>> =
        let elems = Array.ofSeq lst
        // Original impl was to first generate a length and then indices up to the given length.
        // but that skews towards shorter lists. This implementation generates a bitmask up to the
        // length of the input list, and so all possible sublists are equally likely.
        arrayOfLength elems.Length bool
        |> map (fun bitmask -> 
            [ for i in 0..bitmask.Length-1 do
                if bitmask.[i] then yield elems.[i] ])

    ///Generates option values that are 'None' 1/8 of the time.
    //[category: Creating generators from generators]
    let optionOf g :Gen<option<'T>> = frequency [(1, constant None); (7, map Some g)]
    
    // struct type to wrap the keys in the pureFunction cache -
    // to deal with caching null keys.
    [<Struct>]
    type private Key<'K> = private Key of 'K

    let internal pureFunction<'T,'U when 'T:equality> ((Gen g):Gen<'U>) :Gen<'T->'U> =
        let getOrUpdate (from:'T) (createTo:unit->'U) (cache:Dictionary<_,_>) =
                lock cache (fun _ ->
                    let key = Key from
                    let (found,result) = cache.TryGetValue (key)
                    if found then
                        result
                    else
                        let v = createTo()
                        cache.Add(key,v)
                        v)
        Gen (fun s r0 ->
            let mutable r1 = Rnd()
            let mutable r2 = Rnd()
            Random.Split(r0,&r1,&r2)
            let cache = new Dictionary<Key<'T>,'U>()
            let mutable r = r1
            struct ((fun a -> getOrUpdate a (fun () -> let struct (v,r') = g s r in r <- r';v) cache), r2))


[<AutoOpen>]
module GenOperators =
    let inline (<!>) (f:'T->'U) gen = Gen.map f gen
    let inline (<*>) (f:Gen<'T->'U>) gen = Gen.apply f gen
    let inline (>>=) gen (f:'T->Gen<'U>) = Gen.bind f gen

(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2013 Kurt Schelfthout. All rights reserved.          **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

open Random

type internal IGen = 
    abstract AsGenObject : Gen<obj>
    
///Generator of a random value, based on a size parameter and a randomly generated int.
and [<NoEquality;NoComparison>] Gen<'a> = 
    internal Gen of (int -> StdGen -> 'a)
        ///map the given function to the value in the generator, yielding a new generator of the result type.  
        member internal x.Map<'a,'b> (f: 'a -> 'b) : Gen<'b> = match x with (Gen g) -> Gen (fun n r -> f <| g n r)
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
    default x.Shrinker a = 
        Seq.empty
    interface IArbitrary with
        member x.GeneratorObj = (x.Generator :> IGen).AsGenObject
        member x.ShrinkerObj o = (x.Shrinker (unbox o)) |> Seq.map box

///Computation expression builder for Gen.
[<AutoOpen>]
module GenBuilder =

    let private ``return`` x = Gen (fun n r -> x)

    let private bind ((Gen m) : Gen<_>) (k : _ -> Gen<_>) : Gen<_> = 
        Gen (fun n r0 -> let r1,r2 = split r0
                         let (Gen m') = k (m n r1) 
                         m' n r2)

    let private delay (f : unit -> Gen<_>) : Gen<_> = 
        Gen (fun n r -> match f() with (Gen g) -> g n r)

    let rec private doWhile p m =
      if p() then
        bind m (fun _ -> doWhile p m)
      else
        ``return`` ()

    let private tryFinally (Gen m) handler = 
        Gen (fun n r -> try m n r finally handler ())

    let private dispose (x: #System.IDisposable) = x.Dispose()

    let private using r f = tryFinally (f r) (fun () -> dispose r)

    ///The workflow type for generators.
    type GenBuilder internal() =
        member b.Return(a) : Gen<_> = ``return`` a
        member b.Bind(m, k) : Gen<_> = bind m k                            
        member b.Delay(f) : Gen<_> = delay f
        member b.Combine(m1, m2) = bind m1 (fun () -> m2)
        member b.TryFinally(m, handler) = tryFinally m handler
        member b.TryWith(Gen m, handler) = 
            Gen (fun n r -> try m n r with e -> handler e)
        member b.Using (a, k) =  using a k
        member b.ReturnFrom (g:Gen<_>) = g
        member b.While(p, m:Gen<_>) = doWhile p m
        member b.For(s:#seq<_>, f:('a -> Gen<'b>)) =
          using (s.GetEnumerator()) (fun ie ->
            doWhile (fun () -> ie.MoveNext()) (delay (fun () -> f ie.Current))
          )
        member b.Zero() = ``return`` ()

    ///The workflow function for generators, e.g. gen { ... }
    let gen = GenBuilder()

///Combinators to build custom random generators for any type.
module Gen =

    open Common
    open Random
    open Reflect
    open System
    open System.Reflection
    open System.Collections.Generic
    open TypeClass

    ///Apply ('map') the function f on the value in the generator, yielding a new generator.
    let map f (gen:Gen<_>) = gen.Map f

    ///Obtain the current size. sized g calls g, passing it the current size as a parameter.
    let sized fgen = Gen (fun n r -> let (Gen m) = fgen n in m n r)

    ///Override the current size of the test. resize n g invokes generator g with size parameter n.
    let resize n (Gen m) = Gen (fun _ r -> m n r)

    ///Generates a random number generator. Useful for starting off the process
    ///of generating a random value.
    let internal rand = Gen (fun n r -> r)

    ///Generates a value with maximum size n.
    let eval n rnd (Gen m) = 
        let size,rnd' = range (0,n) rnd
        m size rnd'

    ///Generates n values of the given size.
    let sample size n gn  = 
        let rec sample i seed samples =
            if i = 0 then samples
            else sample (i-1) (Random.stdSplit seed |> snd) (eval size seed gn :: samples)
        sample n (Random.newSeed()) []

    ///Generates an integer between l and h, inclusive.
    let choose (l, h) = rand |> map (range (l,h) >> fst) 

    ///Build a generator that randomly generates one of the values in the given non-empty seq.
    let elements xs = 
        choose (0, (Seq.length xs)-1)  |> map(flip Seq.nth xs)

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///equal probability.
    let oneof gens = gen.Bind(elements gens, id)

    ///Build a generator that generates a value from one of the generators in the given non-empty seq, with
    ///given probabilities. The sum of the probabilities must be larger than zero.
    let frequency xs = 
        let tot = Seq.sumBy fst xs
        let rec pick n ys = 
            let (k,x),xs = Seq.head ys,Seq.skip 1 ys
            if n<=k then x else pick (n-k) xs
        in gen.Bind(choose (1,tot), fun n -> pick n xs) 

    ///Map the given function over values to a function over generators of those values.
    let map2 f = fun a b -> gen {   let! a' = a
                                    let! b' = b
                                    return f a' b' }
                                        
    ///Build a generator that generates a 2-tuple of the values generated by the given generator.
    let two g = map2 (fun a b -> (a,b)) g g

    ///Map the given function over values to a function over generators of those values.
    let map3 f = fun a b c -> gen { let! a' = a
                                    let! b' = b
                                    let! c' = c
                                    return f a' b' c' }

    ///Build a generator that generates a 3-tuple of the values generated by the given generator.
    let three g = map3 (fun a b c -> (a,b,c)) g g g

    ///Map the given function over values to a function over generators of those values.
    let map4 f = fun a b c d -> gen {   let! a' = a
                                        let! b' = b
                                        let! c' = c
                                        let! d' = d
                                        return f a' b' c' d' }

    ///Build a generator that generates a 4-tuple of the values generated by the given generator.
    let four g = map4 (fun a b c d -> (a,b,c,d)) g g g g

    ///Map the given function over values to a function over generators of those values.
    let map5 f = fun a b c d e -> gen {  let! a' = a
                                         let! b' = b
                                         let! c' = c
                                         let! d' = d
                                         let! e' = e
                                         return f a' b' c' d' e'}

    ///Map the given function over values to a function over generators of those values.
    let map6 f = fun a b c d e g -> gen {   let! a' = a
                                            let! b' = b
                                            let! c' = c
                                            let! d' = d
                                            let! e' = e
                                            let! g' = g
                                            return f a' b' c' d' e' g'}

    ///Sequence the given list of generators into a generator of a list.
    let sequence l = 
        let rec go gs acc size r0 = 
            match gs with
            | [] -> List.rev acc
            | (Gen g)::gs' ->
                let r1,r2 = split r0
                let y = g size r1
                go gs' (y::acc) size r2
        Gen(fun n r -> go l [] n r)

    ///Generates a list of given length, containing values generated by the given generator.
    let listOfLength n arb = sequence [ for i in 1..n -> arb ]

    ///Tries to generate a value that satisfies a predicate. This function 'gives up' by generating None
    ///if the given original generator did not generate any values that satisfied the predicate, after trying to
    ///get values from by increasing its size.
    let suchThatOption p gn =
        let rec tryValue k n =
            match (k,n) with 
            | (_,0) -> gen {return None }
            | (k,n) -> gen {let! x = resize (2*k+n) gn
                            if p x then return Some x else return! tryValue (k+1) (n-1) }
        sized (tryValue 0 << max 1)


    ///Generates a value that satisfies a predicate. Contrary to suchThatOption, this function keeps re-trying
    ///by increasing the size of the original generator ad infinitum.  Make sure there is a high chance that 
    ///the predicate is satisfied.
    let rec suchThat p gn =
        gen {   let! mx = suchThatOption p gn
                match mx with
                | Some x    -> return x
                | None      -> return! sized (fun n -> resize (n+1) (suchThat p gn)) }

//    /// Takes a list of increasing size, and chooses
//    /// among an initial segment of the list. The size of this initial
//    /// segment increases with the size parameter.
//    /// The input list must be non-empty.
//    let growingElements xs =
//        match xs with
//        | [] -> failwith "subListOf used with empty list"
//        | xs ->
//            let k = List.length xs |> float
//            let mx = 100.0
//            let log' = round << log
//            let size n = ((log' (float n) + 1.0) * k ) / (log' mx) |> int
//            sized (fun n -> elements (xs |> Seq.take (max 1 (size n)) |> Seq.toList))
//    //     TODO check that this does not try choose from segments longer than the original
//    //     it seems that mx indicates the maximum size that the resulting generator can be called with


    /// Generates a list of random length. The maximum length depends on the
    /// size parameter.
    let listOf gn =
        sized <| fun n ->
            gen {   let! k = choose (0,n+1) //decrease chance of empty list
                    return! listOfLength k gn }

    /// Generates a non-empty list of random length. The maximum length 
    /// depends on the size parameter.
    let nonEmptyListOf gn =
        sized <| fun n ->
            gen {   let! k = choose (1,max 1 n)
                    return! listOfLength k gn }

    /// Generates sublists of the given sequence.
    let subListOf l =
        let elems = Array.ofSeq l
        gen {// Generate indices into the array (up to the number of elements)
             let! size = choose(0, elems.Length-1)
             let! indices = listOfLength size (choose(0, elems.Length-1)) 
             let subSeq = indices |> Seq.distinct |> Seq.map (fun i -> elems.[i])
             return List.ofSeq subSeq }

    /// Generates an array of a specified length.
    let arrayOfLength n (g: Gen<'a>) : Gen<'a[]> = listOfLength n g |> map Array.ofList

    /// Generates an array using the specified generator. The maximum length is the size+1.
    let arrayOf (g: Gen<'a>) : Gen<'a[]> = 
       sized <| fun n ->
             gen { let! size = choose(0, n+1) //deccrease chance of empty arrays somewhat
                   return! arrayOfLength size g }

    /// Generates a 2D array of the given dimensions.
    let array2DOfDim (rows: int,cols: int) (g: Gen<'a>)  = 
        gen { let! arr1 = arrayOfLength (rows * cols) g
              return Array2D.init rows cols (fun r c -> arr1.[cols*r + c]) }

    /// Generates a 2D array. The square root of the size is the maximum number of rows and columns.
    let array2DOf (g: Gen<'a>) = 
        sized <| fun n ->
            gen { let chooseSqrtOfSize = choose(0, n |> float |> sqrt |> int)
                  let! rows = chooseSqrtOfSize 
                  let! cols = chooseSqrtOfSize
                  return! array2DOfDim (rows,cols) g }
        
    ///Always generate v.          
    let constant v = gen { return v }

    ///Apply the given Gen function to the given generator, aka the applicative <*> operator.
    let apply (f:Gen<'a -> 'b>) (gn:Gen<'a>) : Gen<'b> =
        gen { let! f' = f
              let! gn' = gn
              return f' gn' }

    ///Promote the given function f to a function generator. Only used for generating arbitrary functions.
    let internal promote f = Gen (fun n r -> fun a -> let (Gen m) = f a in m n r)

    ///Basic co-arbitrary generator transformer, which is dependent on an int.
    ///Only used for generating arbitrary functions.
    let internal variant (v:'a) (Gen m) =
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
        Gen (fun n r -> m n (Seq.nth ((mapToInt v)+1) (rands r)))

///Operators for Gen.
[<AutoOpen>]
module GenOperators =

    /// Lifted function application = apply f to a, all in the Gen applicative functor.
    let (<*>) f a = Gen.apply f a

    /// Like <*>, but puts f in a Gen first.
    let (<!>) f a = Gen.constant f <*> a
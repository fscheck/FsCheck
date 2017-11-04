open System
open System.Reflection
open Microsoft.FSharp.Reflection

#r "bin\Release\FsCheck.dll"
open FsCheck
open FsCheck.Experimental
open System.Collections.Generic
open System.Data

module ShrinkStream =
    type Context<'T> = {
        Next: 'T -> unit
        Stop: unit -> unit
    }
    
    [<Struct>]
    type ShrinkResult = Success | Fail

    type ICall = 
        abstract Shrink: ShrinkResult -> unit
        abstract Constant: unit -> unit

    type ShrinkStream<'T> = ShrinkStream of (Context<'T> -> ICall)

    let noShrink a :ShrinkStream<'T> = 
        fun ctx ->
            { new ICall with
                member __.Shrink _ = ctx.Stop()
                member __.Constant () = ctx.Next a }
        |> ShrinkStream

    let shrinks a shrinker :ShrinkStream<'T> =
        fun ctx ->
            let mutable current = a
            let mutable options :'T[] = null
            let mutable optionIndex = 0
            let nextOrStop() =
                if optionIndex >= options.Length then 
                    ctx.Stop()
                else
                    current <- options.[optionIndex]
                    ctx.Next current
            { new ICall with
                member __.Shrink res =
                    match res with
                    | Success -> 
                        options <- shrinker current
                        optionIndex <- 0
                    | Fail ->
                        optionIndex <- optionIndex + 1
                    nextOrStop()
                member __.Constant () = 
                    ctx.Next current }
        |> ShrinkStream

    let map (f:'T->'U) (ShrinkStream source:ShrinkStream<'T>) :ShrinkStream<'U> =
        fun ctx ->
            source { Next = fun t -> ctx.Next (f t)
                     Stop = ctx.Stop }
        |> ShrinkStream

    let join (ShrinkStream sources:ShrinkStream<ShrinkStream<'T>>) : ShrinkStream<'T> =
        fun ctx ->
            let nextOuter = { Next = fun (ShrinkStream str) -> (str ctx).Constant()
                              Stop = ctx.Stop }
            //let nextInner = { Next = ctx.Next
            { new ICall with
                override __.Constant () = (sources nextOuter).Constant()
                override __.Shrink shrinkResult = () }
            //sources { Next = fun (ShrinkStream inner) -> (inner ctx).
        |> ShrinkStream

    //let bind (ShrinkStream source:ShrinkStream<'T>) (mapping:'T -> ShrinkStream<'U>) :ShrinkStream<'U> =
    //    fun ctx ->
    //        let res = ShrinkResult.Success
    //        source { Next = fun elem -> let (ShrinkStream u) = mapping elem in (u ctx).Constant() // constant because keep the inner prop constant makes most sense
    //                 Stop = fun () -> (s ctx).Shrink res }  // can start shrinking inner?

    //    |> ShrinkStream


    //join = concat = map + bind    

type Size = int

// this is the "local" state of the gen computation.
type Context<'T> = { 
    Next: 'T -> unit
    Reject: unit -> unit
    Size: Size ref
    Seed: Rnd ref }

// each time this function is called, Context.Next is called with the next value,
// and the context is changed.
type CallNext = unit -> unit

type GenStream<'T> = GenStream of (Context<'T> -> CallNext)

let choose (lo,hi) =
    fun ctx -> 
        fun () -> ctx.Next <| Random.rangeInt(lo, hi, !ctx.Seed, ctx.Seed)
    |> GenStream

let constant (t:'T) : GenStream<'T> =
    fun ctx -> 
        fun () -> ctx.Next t
    |> GenStream

let reject : GenStream<'T> =
    fun ctx -> 
        fun () -> ctx.Reject ()
    |> GenStream

let resize size gen : GenStream<'T> =
    fun ctx -> 
        let original = !ctx.Size
        try
            ctx.Size := size
            gen ctx
        finally
            ctx.Size := original
    |> GenStream

let sized (sfun:Size -> GenStream<'T>) : GenStream<'T> =
    GenStream (fun ctx -> let (GenStream g) = sfun !ctx.Size in g ctx)

//// don't think this one is very useful.
//let scan (folder:'State -> 'T -> 'State) (state:'State) (GenStream source) : GenStream<'State> =
//    // shrinking here technically works but is probably not what you want...smaller elements from
//    // source are generated but they're still added to the existing state.
//    // perhaps it's worth building in more of a memory here of all the 'Ts and then try to leave some out
//    // like for lists?
//    fun ctx ->
//        let mutable st = state
//        source { Next = fun e -> st <- folder st e; ctx.Next st
//                 Reject = ctx.Reject
//                 EndShrink = ctx.EndShrink
//                 Size = ctx.Size
//                 Seed = ctx.Seed }
//    |> GenStream

let inline withNext ctx next =
    { Next = next
      Reject = ctx.Reject
      Size = ctx.Size
      Seed = ctx.Seed }

let map (f:'T->'U) (GenStream src) : GenStream<'U> =
    fun ctx ->
        src (withNext ctx <| fun a -> ctx.Next (f a))
    |> GenStream

let where (f:'T->bool) (GenStream src) : GenStream<'T> =
    fun ctx ->
        src { ctx with Next = fun a -> if f a then ctx.Next a else ctx.Reject() }
    |> GenStream

let apply (GenStream f) ((GenStream a):GenStream<'T>) : GenStream<'U> =
    fun ctx ->
        f (withNext ctx <| fun f -> a (withNext ctx <| fun a -> ctx.Next (f a)) ())
    |> GenStream    
    
let collect (mapping:'T -> GenStream<'U>) (GenStream source) : GenStream<'U> =
    fun ctx ->
        source (withNext ctx <| fun t -> let (GenStream g) = mapping t in g ctx ())
    |> GenStream

let inline bind (m:GenStream<'T>) (k:'T->GenStream<'U>) = collect k m

type GenStream<'T> with
    static member inline (>>=)(m,k) = bind m k
    static member inline (<*>)(f,a) = apply f a
    static member inline (<!>)(f,a) = map f a

let sequence (gens:GenStream<'T> list) : GenStream<'T list> =
    fun ctx ->
        let mutable element = Unchecked.defaultof<'T>
        let context = { Size = ctx.Size; Seed = ctx.Seed; Reject=ctx.Reject; Next = fun r -> element <- r }
        let nexts = gens |> List.map (fun (GenStream gen) -> gen context)
        fun () -> ctx.Next [ for next in nexts do next(); yield element ]
    |> GenStream

let sequenceToArr (gens:GenStream<'T>[]) : GenStream<'T[]> =
    fun ctx ->
        let mutable element = Unchecked.defaultof<'T>
        let context = { Size = ctx.Size; Seed = ctx.Seed; Reject=ctx.Reject; Next = fun r -> element <- r }
        let nexts = gens |> Array.map (fun (GenStream gen) -> gen context)
        fun () -> ctx.Next [| for next in nexts do next(); yield element |]
    |> GenStream

let debug s (GenStream gen) : GenStream<'T> =
    fun ctx -> 
        let next t = 
            printfn "before %s seed=%A size=%i" s !ctx.Seed !ctx.Size
            ctx.Next t
            printfn "after %s seed=%A size=%i" s !ctx.Seed !ctx.Size
            
        gen { ctx with Next = next }
    |> GenStream

[<AutoOpen>]
module GenStreamBuilder =
    
    let inline private doWhile p ((GenStream gen):GenStream<'T>) : GenStream<unit> =
        fun { Next=next; Seed=seed; Size=size } ->
            let ignoreCtx = { Next = fun _ -> ()
                              Reject = id
                              Seed = seed
                              Size = size }
            fun () -> while p() do gen ignoreCtx ()
                      next()
        |> GenStream

    let nocurry = ignore false

    let inline private delay (f : unit -> GenStream<'T>) : GenStream<'T> = 
        GenStream (fun ctx -> fun () -> let (GenStream g) = f () in g ctx ())
        // note: this is NOT equivalent to fun ctx -> f () ctx in the presence of side effects
        // in the latter case side-effects get executed only once

    ///The workflow type for generators.
    type GenStreamBuilder internal() =
        member __.Return(a) = constant a
        member __.Bind(m, k) : GenStream<_> = bind m k
        member __.Delay(f) : GenStream<_> = delay f
        member __.Combine(m1, m2) = bind m1 (fun () -> m2)
        member __.ReturnFrom (g:GenStream<_>) = g
        member __.While(p, m:GenStream<_>) = doWhile p m
        member __.For(s:#seq<_>, f:('a -> GenStream<'b>)) =
          using (s.GetEnumerator()) (fun ie ->
            doWhile (fun () -> ie.MoveNext()) (delay (fun () -> f ie.Current))
          )
        member __.Zero() = constant ()

    ///The workflow function for generators, e.g. gen { ... }
    let rnd = GenStreamBuilder()

let scanResults seed size ((GenStream gen):GenStream<'T>) =
    let result = ref Unchecked.defaultof<'T>
    let rejected = ref 0
    let mutable haveResult = false
    let one =  gen { Next = fun r -> haveResult <- true; result := r
                     Reject = fun () -> rejected := !rejected + 1;
                     Size = ref size
                     Seed = ref seed  }
    let next() =
        while not haveResult do one()
        haveResult <- false
    next, result, rejected

let toSeq seed size (gen:GenStream<'T>) : seq<'T> =
    let next, result, _ = scanResults seed size gen
    Seq.initInfinite (fun _ -> next(); !result)

let sampleWithSeed seed size nbSamples (gen:GenStream<'T>) : 'T[] =
    let next, result, _ = scanResults seed size gen
    [| for i in 0..nbSamples-1 do 
         next()
         yield !result |]

let sampleWithSeedRej seed size nbSamples (gen:GenStream<'T>) : 'T[] * int =
    let next, result, rejected = scanResults seed size gen
    [| for i in 0..nbSamples-1 do 
         next()
         yield !result |], !rejected

let inline shuffleInPlace (arr: _ array) =
    // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
    let inline swap (arr : _ array) i j =
        let v = arr.[j]
        arr.[j] <- arr.[i]
        arr.[i] <- v
    rnd {
        let maxI = arr.Length - 1
        let! indexes =
            [| for i in 0..maxI - 1 -> choose (i, maxI) |]
            |> sequenceToArr
        indexes |> Array.iteri (swap arr)
        return arr
    }
    
let piles k sum = 
    let genSorted k p n m =
        rnd {
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
        rnd.Bind(genSorted k k sum sum, shuffleInPlace)
  
let listOf (GenStream gen) : GenStream<list<'T>> =
    sized <| fun n ->
        rnd { let! k = choose (0,n)
              let! sizes = piles k n
              return! sequence [ for size in sizes -> resize size gen ] }

let seed = Random.create()
let size = 100
let nbSamples = 1_000_000

// a recursive implementation of sequence to prove that the stack doesn't overflow
//let rec sqc l =
//    match l with
//    | [] -> constant []
//    | m::ms ->
//        bind m (fun x ->
//            bind (sqc ms) (fun xs -> constant (x::xs)))

//let r = sqc [ for i in 1..1_000_000_000 -> constant i]

//r |> sampleWithSeed seed size 1

let test =
    choose(1,10) >>= fun a ->
    if a < 2 then reject else constant a

test |> sampleWithSeedRej seed 100 1000

let list1 = 
    choose (0,100)
    |> listOf
    |> sampleWithSeed seed size nbSamples

let list2 = 
    Gen.choose (0,100)
    |> Gen.listOf
    |> Gen.sampleWithSeed seed size nbSamples

let s1 = choose (0,100) 
         |> where (fun i -> i < 10) 
         |> collect (fun i -> choose(-i,i))
         |> sampleWithSeed seed size nbSamples

let s2 = Gen.choose (0,100) 
         |> Gen.where (fun i -> i < 10) 
         |> fun g -> gen.Bind(g, fun i -> Gen.choose(-i,i)) 
         |> Gen.sampleWithSeed seed size nbSamples

let seq1 = [ for i in 1..10 -> choose(0,i) ] |> sequence |> sampleWithSeed seed size nbSamples
let seq2 = [ for i in 1..10 -> Gen.choose(0,i) ] |> Gen.sequence |> Gen.sampleWithSeed seed size nbSamples

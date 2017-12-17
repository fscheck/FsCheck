open System
open System.Reflection
open Microsoft.FSharp.Reflection

#r @"bin\Release\FsCheck.dll"
open FsCheck
open FsCheck.Experimental
open System.Collections.Generic
open System.Data

module ShrinkStream =
    type Context<'T> = {
        Next: 'T -> unit
        //Root: 'T ref
    }

    type ICall = 
        /// Move to the next possible shrink and call Next 
        /// if there is one. If there are no more shrinks, don't
        /// call Next.
        abstract Shrink: unit -> unit
        /// Take the current value as start for a new set of shrink
        /// possibilities.
        abstract Root: unit -> unit
        /// Call Next with the current value. 
        abstract Current: unit -> unit

    type ShrinkStream<'T> = ShrinkStream of (Context<'T> -> ICall)

    let noShrink a : ShrinkStream<'T> = 
        fun ctx ->
            { new ICall with
                member __.Shrink () = ()
                member __.Root () = ()
                member __.Current () = ctx.Next a
            }
        |> ShrinkStream

    let shrinks a shrinker : ShrinkStream<'T> =
        fun ctx ->
            let mutable currentRoot = a
            let mutable current = currentRoot
            let mutable options :'T[] = shrinker currentRoot
            let mutable optionIndex = 0                
            { new ICall with
                member __.Shrink () =
                    if optionIndex < options.Length then
                        current <- options.[optionIndex]
                        ctx.Next current
                        optionIndex <- optionIndex + 1
                member __.Root () =
                    currentRoot <- current
                    options <- shrinker current
                    optionIndex <- 0
                member __.Current () = 
                    ctx.Next current 
            }
        |> ShrinkStream

    let map (f:'T->'U) (ShrinkStream source:ShrinkStream<'T>) :ShrinkStream<'U> =
        fun ctx ->
            source { Next = fun t -> ctx.Next (f t) }
        |> ShrinkStream

    let inline private withNext ctx nextFun =
        { Next = nextFun }
    
    let apply (ShrinkStream f:ShrinkStream<'T -> 'U>) (ShrinkStream a:ShrinkStream<'T>) :ShrinkStream<'U> =
        fun ctx ->
            let mutable called = false
            let mutable lastF = Unchecked.defaultof<'T -> 'U>
            let f = f (withNext ctx (fun f -> lastF <- f; called <- true))
            let inline tryShrinkF () = 
                called <- false
                f.Shrink()
                called
            let mutable lastA = Unchecked.defaultof<'T>
            let a = a (withNext ctx (fun a -> lastA <- a; called <- true))
            let inline tryShrinkA () = 
                called <- false
                a.Shrink()
                called
            let inline callNext() =
                ctx.Next (lastF lastA)
            { new ICall with
                member __.Shrink () = 
                    if tryShrinkF() then
                        callNext()
                    elif tryShrinkA() then
                        callNext()
                member __.Root () = 
                    f.Root()
                    a.Root()
                member __.Current () =
                    callNext()
            }
        |> ShrinkStream
    
    let join (ShrinkStream sources:ShrinkStream<ShrinkStream<'T>>) : ShrinkStream<'T> =
        fun ctx ->
            let innerStreams = ResizeArray<ICall>()
            let mutable innerStreamsComplete = [||]
            let mutable innerAdded = false
            let mutable innerCalled = false
            let sourcesNext = sources { Next = fun (ShrinkStream inner) -> 
                innerStreams.Add (inner { Next = (fun t -> ctx.Next t; innerCalled <- true) })
                innerAdded <- true }
            let tryShrinkOuter() =
                innerAdded <- false
                sourcesNext.Shrink()
                innerAdded
            let tryShrinkInner i =
                innerCalled <- false
                innerStreams.[i].Shrink()
                innerStreamsComplete.[i] <- not innerCalled
                innerCalled
            let mutable shrinkingOuter = true
            let mutable innerIndex = 0
            { new ICall with
                member __.Shrink() =
                    if shrinkingOuter && tryShrinkOuter() then
                        innerStreams.[innerStreams.Count-1].Current()
                    elif innerIndex < innerStreams.Count then
                        if shrinkingOuter then //first call into inner shinkers - outer shrinking complete
                            shrinkingOuter <- false
                            innerStreamsComplete <- Array.create innerStreams.Count false
                        while not innerStreamsComplete.[innerIndex] 
                                && innerIndex < innerStreams.Count 
                                && not (tryShrinkInner innerIndex) do
                            innerIndex <- innerIndex + 1
                        
                        
                    
                    // here iterate through the tree of shrink possibilities, breadth first
                    // i.e. take one element at a time off each of the inner streams until they are all exhausted
                member __.Root() = ()
                    //maybe:
                    //sources.Root()
                    //let (ShrinkStream g) = lastInner
                    //(g ctx).Root()
                member __.Current() =
                    (sources { Next = fun (ShrinkStream inner) -> (inner ctx).Current() }).Current()
            }
            //let constantOuter = { Next = fun (ShrinkStream str) -> (str ctx).Constant()
            //                      Stop = ctx.Stop }
            //let shrinkOuter = { Next = fun (ShrinkStream str) -> (str ctx).Shring
            //{ new ICall with
            //    override __.Constant () = (sources constantOuter).Constant()
            //    override __.Shrink shrinkResult = 
            //        sources (nextOuter
            //}
            //sources { Next = fun (ShrinkStream inner) -> (inner ctx).
        |> ShrinkStream
    
    let bind (source:ShrinkStream<'T>) (mapping:'T -> ShrinkStream<'U>) :ShrinkStream<'U> =
        source |> map mapping |> join  

type Size = int

// this is the "local" state of the gen computation.
type Context<'T> = { 
    Next: 'T -> unit
    Reject: unit -> unit
    Size: Size ref
    Seed: Rnd ref 
    ShrinkRoot : 'T ref }    

// each time this function is called, Context.Next is called with the next value,
// and the context is changed.
type ICallNext = 
    abstract Next: unit -> unit
    abstract Shrink: unit -> unit

type GenStream<'T> = GenStream of (Context<'T> -> ICallNext)

let choose (lo,hi) =
    fun ctx -> 
        let mutable current = lo
        { new ICallNext with 
            member __.Next () = 
                ctx.ShrinkRoot := Random.rangeInt(lo, hi, !ctx.Seed, ctx.Seed)
                ctx.Next !ctx.ShrinkRoot
            member __.Shrink () =  
                if !ctx.ShrinkRoot <> lo then ctx.ShrinkRoot := !ctx.ShrinkRoot - 1
                ctx.Next !ctx.ShrinkRoot } //shrink straight to lo
    |> GenStream

let constant (t:'T) : GenStream<'T> =
    fun ctx -> 
        { new ICallNext with 
            member __.Next () = ctx.Next t
            member __.Shrink () = ctx.Next t }
    |> GenStream

let reject<'T> : GenStream<'T> =
    fun ctx -> 
        { new ICallNext with
            member __.Next () = ctx.Reject ()
            member __.Shrink () = ctx.Reject () }
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

let inline withNext ctx next :Context<'T> =
    { Next = next
      Reject = ctx.Reject
      Size = ctx.Size
      Seed = ctx.Seed
      ShrinkRoot = ref Unchecked.defaultof<'T>
    }

let inline withNextAndShrinkRoot ctx next :Context<'T> =
    { Next = next
      Reject = ctx.Reject
      Size = ctx.Size
      Seed = ctx.Seed
      ShrinkRoot = ctx.ShrinkRoot
    }


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
        let mutable called = false
        let mutable lastF = Unchecked.defaultof<'T -> 'U>
        let f = f (withNext ctx (fun f -> lastF <- f; called <- true))
        let inline tryShrinkF () = 
            called <- false
            f.Shrink()
            called
        let mutable lastA = Unchecked.defaultof<'T>
        let a = a (withNext ctx (fun a -> lastA <- a; called <- true))
        let inline tryShrinkA () = 
            called <- false
            a.Shrink()
            called
        let inline callNext() =
            ctx.Next (lastF lastA)
        { new ICallNext with
            member __.Next () = 
                f.Next()
                a.Next()
                callNext()
            member __.Shrink () = 
                if tryShrinkF() then
                    callNext()
                elif tryShrinkA() then
                    callNext()
        }
    |> GenStream    
    
let collect (mapping:'T -> GenStream<'U>) (GenStream source) : GenStream<'U> =
    fun ctx ->
        let mutable called = false
        let mutable lastGenStreamU = Unchecked.defaultof<Context<'U> -> ICallNext>
        let mutable lastU = Unchecked.defaultof<'U>
        let mutable lastT = Unchecked.defaultof<'T>
        let inline tryShrinkSource () = 
            called <- false
            (source (withNext ctx (fun t -> lastT <- t; called <- true))).Shrink()
            called        
        let inline tryShrinkU () = 
            called <- false
            (lastGenStreamU (withNextAndShrinkRoot ctx (fun a -> lastU <- a; called <- true))).Shrink()
            called
        let inline callNext() =
            ctx.Next lastU
        let inline callMapping () =
            let str = mapping lastT
            let (GenStream g) = str
            lastGenStreamU <- g
        { new ICallNext with
            member __.Next () = 
                (source (withNext ctx (fun t -> lastT <- t))).Next()
                callMapping()
                (lastGenStreamU (withNext ctx (fun u -> lastU <- u))).Next()
                callNext()
            member __.Shrink () = 
                if tryShrinkSource() then
                    callMapping()
                    (lastGenStreamU (withNextAndShrinkRoot ctx (fun u -> lastU <- u))).Shrink() //tell shrink root is set??
                    callNext()
                elif tryShrinkU() then
                    callNext()
        }
    |> GenStream

let inline bind (m:GenStream<'T>) (k:'T->GenStream<'U>) = collect k m

type GenStream<'T> with
    static member inline (>>=)(m,k) = bind m k
    static member inline (<*>)(f,a) = apply f a
    static member inline (<!>)(f,a) = map f a

let scanResults seed size ((GenStream gen):GenStream<'T>) =
    let result = ref Unchecked.defaultof<'T>
    let rejected = ref 0
    let mutable haveResult = false
    let one =  gen { Next = fun r -> haveResult <- true; result := r
                     Reject = fun () -> rejected := !rejected + 1;
                     Size = ref size
                     Seed = ref seed
                     ShrinkRoot = ref Unchecked.defaultof<'T> }
    let next() =
        while not haveResult do one.Next()
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

let shrink ((GenStream gen):GenStream<'T>) =
    let result = ref Unchecked.defaultof<'T>
    let rejected = ref 0
    let mutable haveResult = false
    let ctx = { Next = fun r -> haveResult <- true; result := r
                Reject = fun () -> rejected := !rejected + 1;
                Size = ref 100
                Seed = ref <| Random.create()
                ShrinkRoot = ref Unchecked.defaultof<'T> }
    let one =  gen ctx
    one.Next()
    [| yield !ctx.ShrinkRoot
       one.Shrink()
       while haveResult do
            haveResult <- false
            yield !ctx.ShrinkRoot |]


let g =
    choose (0,10)
    |> collect (fun i -> choose(-i,i))
    |> shrink

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

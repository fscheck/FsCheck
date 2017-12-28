open System
open System.Reflection
open Microsoft.FSharp.Reflection

#r @"bin\Release\FsCheck.dll"
open FsCheck
open FsCheck.Experimental
open System.Collections.Generic
open System.Data

type Size = int

/// The state of the GenStream computation.
type Context<'T> = { 
    Next: 'T -> unit
    Reject: unit -> unit
    Size: Size ref
    Seed: Rnd ref }

type IStream = 
    /// Each time Generate is called, a new random element is generated
    /// and either Next or Reject is called on the context.
    abstract Generate: unit -> unit
    /// Take the last generated element and use it as the root for subsequent Shrink
    /// calls.
    abstract PrepareShrinkFromCurrent: unit -> unit
    /// Each time Shrink is called, a smaller element than the last generated or shrunk
    /// is found. If it exists, Next or Reject is called. If it does not, nothing is called.
    abstract Shrink: unit -> unit

type GenStream<'T> = GenStream of (Context<'T> -> IStream)

let choose (lo,hi) =
    fun ctx -> 
        let mutable current = lo
        let mutable shrinks = [| |]
        let mutable shrinkIdx = 0
            
        let inline callNext v =
            current <- v
            ctx.Next v

        let inline printState m =
            printfn "%s current=%i shrinks=%A shrinkId=%i" m current shrinks shrinkIdx 

        { new IStream with
            member __.Generate () = 
               //printState "Generate"
               callNext (Random.rangeInt(lo, hi, !ctx.Seed, ctx.Seed))

            member __.Shrink () =  
                //printState "Shrink"
                if shrinkIdx < shrinks.Length then 
                    ctx.Next shrinks.[shrinkIdx]
                    shrinkIdx <- shrinkIdx + 1

            member __.PrepareShrinkFromCurrent () =
                //printState "PrepareShrink"
                let currIdx = shrinkIdx - 1 
                if currIdx >= 0 && currIdx < shrinks.Length then current <- shrinks.[currIdx]
                shrinks <- [| let mutable c = current
                              while c > lo do
                                  c <- c - 1
                                  yield c |]
                shrinkIdx <- 0
        }
    |> GenStream

let constant (t:'T) : GenStream<'T> =
    fun ctx ->
        { new IStream with 
            member __.Generate () = ctx.Next t
            member __.Shrink () = ()
            member __.PrepareShrinkFromCurrent () = () }
    |> GenStream

let reject<'T> : GenStream<'T> =
    fun ctx -> 
        { new IStream with
            member __.Generate () = ctx.Reject ()
            member __.Shrink () = ()
            member __.PrepareShrinkFromCurrent () = ()
        }
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
    }

let map (f:'T->'U) (GenStream src) : GenStream<'U> =
    fun ctx ->
        let mapped = withNext ctx <| fun a -> ctx.Next (f a)
        src mapped
    |> GenStream

let where (f:'T->bool) (GenStream src) : GenStream<'T> =
    fun ctx ->
        let filtered = { ctx with Next = fun a -> if f a then ctx.Next a else ctx.Reject() }
        src filtered
    |> GenStream

let inline withNextReject ctx next reject : Context<'T> =
    { Next = next
      Reject = reject
      Size = ctx.Size
      Seed = ctx.Seed
    }

let apply (GenStream f) ((GenStream a):GenStream<'T>) : GenStream<'U> =
    fun ctx ->
        let mutable called = false
        let mutable rejected = false
        let mutable lastF = Unchecked.defaultof<'T -> 'U>
        let f = f (withNextReject ctx (fun f -> lastF <- f; called <- true)
                                      (fun () -> rejected <- true; ctx.Reject()))
        let inline tryShrinkF () = 
            called <- false
            f.Shrink()
            called
        let mutable lastA = Unchecked.defaultof<'T>
        let a = a (withNextReject ctx (fun a -> lastA <- a; called <- true)
                                      (fun () -> rejected <- true; ctx.Reject()))
        let inline tryShrinkA () = 
            called <- false
            a.Shrink()
            called
        let inline callNext() =
            ctx.Next (lastF lastA)
        { new IStream with
            member __.Generate () = 
                rejected <- false
                f.Generate()
                if not rejected then a.Generate()
                if not rejected then callNext()
            member __.Shrink () = 
                rejected <- false
                if tryShrinkF() then
                    callNext()
                elif not rejected && tryShrinkA() then
                    callNext()
            member __.PrepareShrinkFromCurrent () =
                f.PrepareShrinkFromCurrent()
                a.PrepareShrinkFromCurrent()
        }
    |> GenStream    
 

let collect (mapping:'T -> GenStream<'U>) (GenStream source) : GenStream<'U> =
    fun ctx ->
        let mutable called = false
        let mutable rejected = false
        let mutable lastStreamU = Unchecked.defaultof<IStream>
        let mutable lastU = Unchecked.defaultof<'U>
        let mutable lastT = Unchecked.defaultof<'T>
        let source = source (withNextReject ctx (fun t -> lastT <- t; called <- true)
                                                (fun () -> rejected <- true; ctx.Reject()))
        let uCtx = withNextReject ctx (fun a -> lastU <- a; called <- true)
                                      (fun () -> rejected <- true; ctx.Reject())
        let inline tryShrinkSource() = 
            called <- false
            source.Shrink()
            called
        let inline tryShrinkU() = 
            called <- false
            lastStreamU.Shrink()
            called
        let inline callNext() =
            ctx.Next lastU
        let inline callMapping() =
            let (GenStream g) = mapping lastT
            lastStreamU <- g uCtx
        { new IStream with
            member __.Generate () =
                rejected <- false
                source.Generate()
                if not rejected then
                    callMapping()
                    lastStreamU.Generate()
                if not rejected then 
                    callNext()
            member __.Shrink () = 
                rejected <- false
                if tryShrinkSource() then
                    callMapping()
                    lastStreamU.Generate()
                    callNext()
                elif not rejected && tryShrinkU() then
                    callNext()
            member __.PrepareShrinkFromCurrent() =
                source.PrepareShrinkFromCurrent()
                lastStreamU.PrepareShrinkFromCurrent()
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
                     Seed = ref seed }
    let next() =
        while not haveResult do one.Generate()
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

let shrink check  ((GenStream gen):GenStream<'T>)  =
    let mutable result = Unchecked.defaultof<'T>
    let mutable rejected = 0
    let mutable haveResult = false
    let ctx = { Next = fun r -> haveResult <- true; result <- r
                Reject = fun () -> rejected <- rejected + 1;
                Size = ref 100
                Seed = ref <| Random.create() }
    let one =  gen ctx
    
    [| one.Generate()
       one.PrepareShrinkFromCurrent()
       while haveResult do
            yield result
            haveResult <- false
            one.Shrink()
            if check result then one.PrepareShrinkFromCurrent()
    |]


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
        fun ctx ->
            let ignoreNext = withNext ctx (fun _ -> ()) |> gen
            { new IStream with
                member __.Generate() =
                    while p() do ignoreNext.Generate()
                    ctx.Next ()
                member __.PrepareShrinkFromCurrent() = ()
                member __.Shrink() = ()
            }
        |> GenStream

    let inline private delay (f : unit -> GenStream<'T>) : GenStream<'T> = 
        // f can have side effects so must be executed every time.
        fun ctx ->
            let inline run() =
                let (GenStream g) = f ()
                g ctx
            { new IStream with
                member __.Generate() =
                    let forced = run()
                    forced.Generate()
                member __.Shrink() =
                    let forced = run()
                    forced.Shrink()
                member __.PrepareShrinkFromCurrent() =
                    let forced = run()
                    forced.PrepareShrinkFromCurrent()
            }
        |> GenStream

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

    ///The workflow function for generators.
    let rnd = GenStreamBuilder()

let sequenceArr (gens:GenStream<'T>[]) : GenStream<'T[]> =
    fun ctx ->
        let mutable rejected = false
        let mutable called = false
        let mutable element = Unchecked.defaultof<'T>
        let context = withNextReject ctx (fun r -> element <- r; called <- true) (fun () -> rejected <- true; ctx.Reject())
        let nexts = gens |> Array.map (fun (GenStream g) -> g context)
        let mutable genIdx = 0
        let mutable res = Array.zeroCreate nexts.Length
        let mutable current = Unchecked.defaultof<'T[]>
        let mutable currentIdx = 0
        let inline callNext r =
            current <- r
            ctx.Next current
        { new IStream with
            override __.Generate() =
                rejected <- false
                // build up the result, stop and don't call next if rejected,
                // but keep the state and the result of previous elements.
                while not rejected && genIdx < nexts.Length do
                    let next = nexts.[genIdx]
                    next.Generate()
                    if not rejected then
                        res.[genIdx] <- element
                        genIdx <- genIdx + 1

                if not rejected then
                    callNext res
                    genIdx <- 0
                    res <- Array.zeroCreate nexts.Length
            override __.PrepareShrinkFromCurrent() =
                nexts |> Seq.iter (fun s -> s.PrepareShrinkFromCurrent())
                current <- res
            override __.Shrink() =
                rejected <- false
                called <- false

                while not called && not rejected && currentIdx < nexts.Length do
                    let next = nexts.[currentIdx]
                    next.Shrink()
                    if not rejected then
                        currentIdx <- currentIdx + 1
                
                if called then
                    let res = current |> Array.mapi (fun i e -> if i=currentIdx-1 then element else e)
                    ctx.Next res
                    if currentIdx >= nexts.Length then
                        currentIdx <- 0
        }
    |> GenStream

let sequence (gens:GenStream<'T> list) : GenStream<'T list> =
    gens |> List.toArray |> sequenceArr |> map List.ofArray 

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
            |> sequenceArr
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

//let g =
//    choose (0,20)
//    |> where (fun i -> i > 5)
//    |> map char
//  //  |> collect (fun i -> choose(-i,i))
//    |> shrink (fun v -> int v < 8)

//let g2 =
//    (fun a b c -> (a,b,c)) <!> choose(0,10) <*> choose(30,40) <*> (choose(120,130) |> map char)
//    |> shrink (fun (a,b,c) -> a < 5 && b > 35)

//let g3 =
//    choose (0,20)
//    |> collect (fun i -> choose(-i,i))
//    |> shrink ((=) 0)

open System
open System.Reflection
open Microsoft.FSharp.Reflection

#r @"bin\Release\FsCheck.dll"
open FsCheck
open FsCheck.Experimental
open System.Collections.Generic
open System.Data

type Size = int

type ShrinkCont<'T> = {
    Next: 'T -> unit
    Done: unit -> unit
    Shrinks: ShrinkStream<'T> -> unit
}

and Shrinker = {
    /// Each time Shrink is called, a smaller element than the last generated or shrunk
    /// is found. If it exists, Next is called. If it does not, Done is called.
    Shrink: unit -> unit
    /// Create a new ShrinkStream from the last value. Calls Shrinks with the resulting ShrinkStream.
    GetShrinks: unit -> unit
}

and ShrinkStream<'T> = ShrinkCont<'T> -> Shrinker

module Shrink =

    let rec empty : ShrinkStream<'T> =
        fun ctx ->
            { Shrink = ctx.Done
              GetShrinks = fun () -> ctx.Shrinks empty
            }

    let rec choose current (lo,hi) : ShrinkStream<int> =
        fun ctx ->
            let shrinks = [| let mutable c = current
                             while c > lo do
                                c <- c - 1
                                yield c |]
            let mutable shrinkIdx = 0
            { Shrink = fun () ->  
                        if shrinkIdx < shrinks.Length then 
                            ctx.Next shrinks.[shrinkIdx]
                            shrinkIdx <- shrinkIdx + 1
                        else
                            ctx.Done()
              GetShrinks = fun () ->
                            let currIdx = shrinkIdx - 1
                            let current = if currIdx >= 0 && currIdx < shrinks.Length then shrinks.[currIdx] else current
                            if current = lo then ctx.Shrinks empty else ctx.Shrinks <| choose current (lo,hi)
            }

    let rec map f (str:ShrinkStream<'T>) : ShrinkStream<'U> =
        fun { Next=next; Done=don; Shrinks=shrink } ->
            str { Next = fun e -> next (f e)
                  Done = don
                  Shrinks = fun s -> shrink (map f s)}

    let rec where pred (str:ShrinkStream<'T>) : ShrinkStream<'T> =
        fun ctx ->
            let rec str' = str { ctx with Next = fun e -> if pred e then ctx.Next e else str'.Shrink()
                                          Shrinks = fun s -> ctx.Shrinks (where pred s) }
            str'

    let rec apply currentF currentA (f:ShrinkStream<'T->'U>) (a:ShrinkStream<'T>)  : ShrinkStream<'U> =
        fun { Next=next; Done=don; Shrinks=shrink } ->
            let mutable currentF = currentF
            let mutable currentA = currentA
            let mutable fShrinkStream = f
            let aCtx = { Next = fun e -> currentA <- e; next (currentF currentA)
                         Done = don
                         Shrinks = fun s -> shrink <| apply currentF currentA fShrinkStream s }
            let aStr = lazy a aCtx // for reason for lazy see remark in Gen.apply
            f { Next = fun e -> currentF <- e; next (currentF currentA)
                Done = fun () -> aStr.Value.Shrink() 
                Shrinks = fun s -> fShrinkStream <- s; aStr.Value.GetShrinks() }

    let rec join (strs:ShrinkStream<ShrinkStream<'T>>) : ShrinkStream<'T> =
        fun ctx ->
            let mutable currentInner = Unchecked.defaultof<ShrinkStream<'T>>
            strs { Next = fun str -> currentInner <- str; (str ctx).Shrink() 
                   Done = fun () -> (currentInner ctx).Shrink()
                   Shrinks = fun s -> ctx.Shrinks <| join s }

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
Note in the below it's important that we write
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
being re-evaluated.
*)

type Generator = {  
    /// Each time Generate is called, a new random element is generated
    /// and either Next or Reject is called on the context.
    Generate: unit -> unit
    /// Create a new ShrinkStream from the last
    /// generated element and pass it to Shrinks on the context.
    GetShrinks: unit -> unit
}

type GenStream<'T> = GenStream of (Context<'T> -> Generator)

let choose (lo,hi) =
    fun ctx -> 
        let mutable current = lo            
        let inline callNext v =
            current <- v
            ctx.Next v
        { Generate = fun () -> callNext (Random.rangeInt(lo, hi, !ctx.Seed, ctx.Seed))
          GetShrinks = fun () -> ctx.Shrinks <| Shrink.choose current (lo,hi)
        }
    |> GenStream

let constant (t:'T) : GenStream<'T> =
    fun ctx ->
        { Generate = fun () -> ctx.Next t
          GetShrinks = fun () -> ctx.Shrinks <| Shrink.empty
        }
    |> GenStream

let reject<'T> : GenStream<'T> =
    fun ctx -> 
        { Generate = fun () -> ctx.Reject ()
          GetShrinks = fun () -> ctx.Shrinks <| Shrink.empty
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

let inline withNext ctx next shrink : Context<'T> =
    { Next = next
      Reject = ctx.Reject
      Size = ctx.Size
      Seed = ctx.Seed
      Shrinks = shrink
    }

let map (f:'T->'U) (GenStream src) : GenStream<'U> =
    fun ctx ->
        src <| withNext ctx (fun a -> ctx.Next (f a))
                            (fun str -> ctx.Shrinks (Shrink.map f str))
    |> GenStream

let where (f:'T->bool) (GenStream src) : GenStream<'T> =
    fun ctx ->
        src <| withNext ctx (fun a -> if f a then ctx.Next a else ctx.Reject())
                            (fun s -> ctx.Shrinks <| Shrink.where f s)
    |> GenStream

let inline printStackFrames s =
    let st = new System.Diagnostics.StackTrace()
    printfn "%s %i %s" s st.FrameCount (st.ToString())

let apply (GenStream f) ((GenStream a):GenStream<'T>) : GenStream<'U> =
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

    |> GenStream    
 
 // generate a single element from the stream which is the first shrink; then create the shrinks from that
let shrinks genCtx (GenStream str:GenStream<'T>) : ShrinkStream<'T> =
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

let bind (GenStream m:GenStream<'T>) (k:'T->GenStream<'U>) : GenStream<'U> = 
    fun ctx ->
        let mutable lastStreamU = Unchecked.defaultof<Generator>
        let mutable lastU = Unchecked.defaultof<'U>
        let mutable lastT = Unchecked.defaultof<'T>
        let mutable mShrinkStream = Unchecked.defaultof<ShrinkStream<'T>>

        let uCtx = withNext ctx (fun a -> lastU <- a; ctx.Next lastU) ctx.Shrinks
        let inline setLastStreamU() =
            let (GenStream g) = k lastT
            lastStreamU <- g uCtx
        let m = m (withNext ctx (fun t -> lastT <- t; setLastStreamU(); lastStreamU.Generate())
                                (fun s -> mShrinkStream <- s
                                          let sstr = s |> Shrink.map (k >> shrinks ctx)
                                          ctx.Shrinks <| Shrink.join sstr))
        m
    |> GenStream

let inline collect (mapping:'T -> GenStream<'U>) (source:GenStream<'T>) : GenStream<'U> = bind source mapping

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
                     Shrinks = fun s -> () //not shrinking here at the moment
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

// a tail-recursive implementation of sequence to prove that the stack doesn't overflow
let sqc (l:list<_>) =
    let rec helper inp res =
        match inp with
        | [] -> res
        | m::ms ->
            helper ms (apply (map (fun xs x -> x::xs) res) m)
            // or (slower): helper ms (apply (apply (constant (fun xs x -> x::xs)) res) m)
            // or (slowest): helper (bind m (fun x -> bind res (fun xs -> constant (x::xs))))
    helper l (constant [])

//let r = sqc [ for i in 1..100 -> constant i]
//r |> sampleWithSeed (Random.create()) 100 100_000 |> Seq.take 10

let sampleWithSeedRej seed size nbSamples (gen:GenStream<'T>) : 'T[] * int =
    let next, result, rejected = scanResults seed size gen
    [| for i in 0..nbSamples-1 do 
         next()
         yield !result |], !rejected

let shrink check ((GenStream gen):GenStream<'T>)  =
    let mutable result = Unchecked.defaultof<'T>
    let mutable rejected = 0
    let mutable haveResult = false
    let mutable shrinkStream = Unchecked.defaultof<ShrinkStream<'T>>
    let shrCtx = { Next = fun r -> result <- r
                   Done = fun () -> haveResult <- false
                   Shrinks = fun s -> shrinkStream <- s }
    let rec one  = gen { Next = fun r -> result <- r
                         Reject = fun () -> rejected <- rejected + 1; one.Generate()
                         Shrinks = fun s -> shrinkStream <- s
                         Size = ref 100
                         Seed = ref <| Random.create() }
    
    [| one.Generate()
       one.GetShrinks()
       while haveResult do
            yield result
            haveResult <- false
            (shrinkStream shrCtx).Shrink()
            if check result then (shrinkStream shrCtx).GetShrinks()
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
            let ignoreNext = withNext ctx (fun _ -> ()) (fun _ -> ()) |> gen
            { Generate = fun () ->
                    while p() do ignoreNext.Generate()
                    ctx.Next ()
              GetShrinks = fun () ->
                    ctx.Shrinks Shrink.empty
            }
        |> GenStream

    let inline private delay (f : unit -> GenStream<'T>) : GenStream<'T> = 
        // f can have side effects so must be executed every time.
        fun ctx ->
            let inline run() =
                let (GenStream g) = f ()
                g ctx
            { Generate = fun () ->
                    let forced = run()
                    forced.Generate()
              GetShrinks = fun () ->
                    let forced = run()
                    forced.GetShrinks()
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
                                         
let fold (folder:'State -> 'T -> 'State) (state:GenStream<'State>) (gens:seq<GenStream<'T>>) : GenStream<'State> =
    
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
        { new Generator with
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
            override __.GetShrinks() =
                rejected <- false
                called <- false

                while not called && not rejected && currentIdx < nexts.Length do
                    let next = nexts.[currentIdx]
                    next.GetShrinks()
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

#nowarn "40" // recursive references

namespace FsCheck

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
                            // get next first, so the ctx.Next call is in tail position
                            // (alternative is to increment shrinkIdx after calling ctx.Next)
                            let next = shrinks.[shrinkIdx]
                            shrinkIdx <- shrinkIdx + 1
                            ctx.Next next
                        else
                            ctx.Done()
              GetShrinks = fun () ->
                            let currIdx = shrinkIdx - 1
                            let current = if currIdx >= 0 && currIdx < shrinks.Length then shrinks.[currIdx] else current
                            if current = lo then 
                                ctx.Shrinks empty 
                            else 
                                ctx.Shrinks <| choose current (lo,hi)
            }

    let rec map f (str:ShrinkStream<'T>) : ShrinkStream<'U> =
        fun ctx ->
            str { Next = fun e -> ctx.Next (f e)
                  Done = ctx.Done
                  Shrinks = fun s -> ctx.Shrinks (map f s)}

    let rec where pred (str:ShrinkStream<'T>) : ShrinkStream<'T> =
        fun ctx ->
            let mutable more = false
            let str' = 
                str { ctx with Next = fun e -> if pred e then more <- false; ctx.Next e else more <- true
                               Done = fun () -> more <-false; ctx.Done()
                               Shrinks = fun s -> ctx.Shrinks (where pred s) }
            { Shrink = fun () ->
                str'.Shrink()
                while more do str'.Shrink()
              GetShrinks = str'.GetShrinks }

    let rec apply currentF currentA (f:ShrinkStream<'T->'U>) (a:ShrinkStream<'T>)  : ShrinkStream<'U> =
        fun ctx ->
            let mutable currentF = currentF
            let mutable currentA = currentA
            let mutable fShrinkStream = f
            let aCtx = { Next = fun e -> currentA <- e; ctx.Next (currentF currentA)
                         Done = ctx.Done
                         Shrinks = fun s -> ctx.Shrinks <| apply currentF currentA fShrinkStream s }
            let aStr = lazy a aCtx // for reason for lazy see remark in Gen.apply
            f { Next = fun e -> currentF <- e; ctx.Next (currentF currentA)
                Done = fun () -> aStr.Value.Shrink() 
                Shrinks = fun s -> fShrinkStream <- s; aStr.Value.GetShrinks() }

    let rec join (strs:ShrinkStream<ShrinkStream<'T>>) : ShrinkStream<'T> =
        fun ctx ->
            let mutable currentInner = Unchecked.defaultof<Shrinker>
            strs { Next = fun str -> currentInner <- str ctx; currentInner.Shrink() 
                   Done = fun () -> currentInner.Shrink()
                   Shrinks = fun s -> ctx.Shrinks <| join s }

    /// Shrinks an array by keeping the lenght of the array the same, and shrinking
    /// each element according to the given shrinkers one by one, first to last.
    let rec elements current (shrinkers:ShrinkStream<'T>[]) : ShrinkStream<'T[]> =
        fun ctx ->
            let ts = Array.copy current
            let mutable idx = 0
            let rec sCtx = 
                { Next = fun st -> ts.[idx] <- st
                                   ctx.Next (Array.copy ts)
                  Done = fun () -> ts.[idx] <- current.[idx] // restore prev element
                                   idx <- idx + 1
                                   if idx < ts.Length then shrinkersShrink.[idx].Shrink()
                                   else ctx.Done()
                  Shrinks = fun s -> 
                    let newShrinkers = Array.copy shrinkers
                    newShrinkers.[idx] <- s
                    ctx.Shrinks <| elements ts newShrinkers
                }

            and shrinkersShrink:_[] = shrinkers |> Array.map (fun s -> s sCtx)
            { Shrink = fun () -> 
                if idx < ts.Length then
                    shrinkersShrink.[idx].Shrink()
                else
                    ctx.Done()
              GetShrinks = fun () -> 
                if idx < ts.Length then
                    shrinkersShrink.[idx].GetShrinks()
                else
                    ctx.Shrinks empty
                }

    /// Shrinks an array by keeping the elements the same but successively removing
    /// one element from the array.
    let rec array (current:'T[]) : ShrinkStream<'T[]> =
        fun ctx ->
            let mutable idx = 0
            let mutable next = [||]
            { Shrink = fun () ->
                if idx < current.Length then
                    next <- Array.zeroCreate (current.Length-1)
                    Array.blit current 0 next 0 idx
                    Array.blit current (idx+1) next idx (current.Length-1-idx)
                    idx <- idx + 1
                    ctx.Next next
                else
                    ctx.Done()
              GetShrinks = fun () ->
                ctx.Shrinks <| array next
            }

    /// Append to shrinkstreams - shrinks in the first are tried first, then the second.
    let rec append (s1:ShrinkStream<'T>) (s2:ShrinkStream<'T>) : ShrinkStream<'T> =
        fun ctx ->
            let mutable shrinks1 = Unchecked.defaultof<ShrinkStream<'T>>
            let shrinker2 = s2 { ctx with Shrinks = fun shrinks2 -> ctx.Shrinks <| append shrinks1 shrinks2 }
            s1 { ctx with Done = fun () -> shrinker2.Shrink()
                          Shrinks = fun s -> shrinks1 <-s; shrinker2.GetShrinks()
            }

    /// Shrinks an array by first removing each element in turn, then shrinking each element
    /// according to the given element shrinkers.
    let rec arrayThenElements (current:'T[]) (elems:ShrinkStream<'T>[]) : ShrinkStream<'T[]> =
        append (array current) (elements current elems)
    
    let toSeq (str:ShrinkStream<'T>) =
        let mutable next = Unchecked.defaultof<'T>
        let mutable isDone = false
        let ctx = { Next = fun n -> next <- n
                    Done = fun () -> isDone <- true
                    Shrinks = fun s -> () }
        seq {
            // if you put this let outside of the seq, 
            // it keeps the state of shrinking accross calls
            let shrink = str ctx
            isDone <- false
            shrink.Shrink()
            while not isDone do
                yield next
                shrink.Shrink()
        }
    
    /// Create a ShrinkStream from the given current value and shrinking function.
    let rec shrink (current:'T) (shrinker:'T -> 'T seq) : ShrinkStream<'T> =
        fun ctx ->
            let mutable current = Unchecked.defaultof<'T>
            let mutable enumerator = Seq.empty<'T>.GetEnumerator()
            let ctx = { ctx with Next = (fun n -> current <- n; ctx.Next n) }
            { Shrink = fun () ->
                if enumerator.MoveNext() then 
                    ctx.Next enumerator.Current
                else
                    ctx.Done()
              GetShrinks = fun () ->
                ctx.Shrinks <| shrink current shrinker
            }



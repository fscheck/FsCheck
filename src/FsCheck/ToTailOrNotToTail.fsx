


module NonTailRec =
    
    type C<'T> = { Next: 'T -> unit }

    type Go = 
        abstract CallNext: unit -> unit

    type Str<'T> = C<'T> -> Go

    let constant t : Str<'T> = fun c -> { new Go with member __.CallNext() = c.Next t }

    let bind (m:Str<'T>) (k:'T->Str<'U>) : Str<'U> =
        fun c ->
            let nextM = { Next = fun melem -> ((k melem c).CallNext) () }
            let m = ((m nextM).CallNext)
            { new Go with member __.CallNext() = m() }

    let sequence strs : Str<list<'T>> =
        let rec helper inp res =
            match inp with
            | [] -> res
            | m::ms -> helper ms (bind m (fun x -> bind res (fun xs -> constant (x::xs))))
        helper strs (constant [])

    let sample nb (str:Str<'T>) =
        let mutable elem = Unchecked.defaultof<'T>
        let next = fun e -> elem <- e
        let gen = str { Next = next }
        [| for i in 1..nb do 
            gen.CallNext()
            yield elem
        |]



module TailRec =
    
    type C<'T> = { Next: 'T -> unit }

    type Go = { CallNext: unit -> unit }
            
    type Str<'T> = C<'T> -> Go

    let constant t : Str<'T> = 
        fun c -> 
            { CallNext = fun () -> c.Next t }

    let apply (f:Str<'T->'U>) (a:Str<'T>) : Str<'U> =
        fun c ->
            let mutable lastF = Unchecked.defaultof<'T->'U>
            let f = (f { Next = fun f -> lastF <- f }).CallNext
            let a = (a { Next = fun a -> c.Next <| lastF a }).CallNext
            { CallNext = fun () -> f(); a() }

    let bind (m:Str<'T>) (k:'T->Str<'U>) : Str<'U> =
        fun c ->
            let nextM = { Next = fun melem -> (k melem c).CallNext() }
            let m = (m nextM).CallNext
            { CallNext = m }

    let sequence strs : Str<list<'T>> =
        let rec helper inp res =
            match inp with
            | [] -> res
            | m::ms -> helper ms (bind m (fun x -> bind res (fun xs -> constant (x::xs))))
        helper strs (constant [])

    let sequence2 strs : Str<list<'T>> =
        let rec helper inp res =
            match inp with
            | [] -> res
            | m::ms -> helper ms (apply (apply (constant <| fun x xs -> x::xs) m) res)
        helper strs (constant [])

    let sample nb (str:Str<'T>) =
        let mutable elem = Unchecked.defaultof<'T>
        let next = fun e -> elem <- e
        let gen = str { Next = next }
        [| for i in 1..nb do 
            gen.CallNext()
            yield elem
        |]

module TailRecTrampoline =
    
    open System.Collections.Generic

    type Thunk = unit -> unit

    type C<'T> = { Next: 'T -> unit 
                   Thunks: Queue<Thunk> }

    type Go = unit
            
    type Str<'T> = C<'T> -> Go

    let trampoline (thunks:Queue<Thunk>) =
        let mutable c = 0
        while not (thunks.Count = 0) do
            let t = thunks.Dequeue()
            printfn "%i %A" c t
            t()
            c <- c + 1

    let constant t : Str<'T> = 
        fun c -> 
            c.Thunks.Enqueue <| fun () -> printfn "constant %A" c.Thunks; c.Next t

    let mutable counter = 0

    let apply (f:Str<'T->'U>) (a:Str<'T>) : Str<'U> =
        fun c ->
            let mutable lastF = Unchecked.defaultof<'T->'U>
            let fCount = counter
            counter <- counter + 1
            printfn "new lastF %i" fCount
            let fCtx = { Next = fun f -> printfn "Set lastF %i to %A" fCount f; lastF <- f
                         Thunks = c.Thunks }
            let aCtx = { Next = fun a -> printfn "lastF %i %A" fCount lastF; c.Next <| lastF a 
                         Thunks = c.Thunks }
            c.Thunks.Enqueue <| fun () -> printfn "f a %A" c.Thunks
                                          f fCtx
                                          a aCtx

    //let bind (m:Str<'T>) (k:'T->Str<'U>) : Str<'U> =
    //    fun c ->
    //        let nextM = { Next = fun melem -> (k melem c).CallNext() }
    //        let m = (m nextM).CallNext
    //        { CallNext = m }

    //let sequence strs : Str<list<'T>> =
    //    let rec helper inp res =
    //        match inp with
    //        | [] -> res
    //        | m::ms -> helper ms (bind m (fun x -> bind res (fun xs -> constant (x::xs))))
    //    helper strs (constant [])

    let sequence2 strs : Str<list<'T>> =
        let rec helper inp res =
            match inp with
            | [] -> res
            | m::ms -> helper ms (apply (apply (constant <| fun x xs -> x::xs) m) res)
        helper strs (constant [])

    let sample nb (str:Str<'T>) =
        let mutable elem = Unchecked.defaultof<'T>
        let next = fun e -> elem <- e
        let ctx = { Next = next; Thunks = new Queue<Thunk>() }
        let gen = str ctx
        [| for i in 1..nb do 
            trampoline  ctx.Thunks
            yield elem
        |]
    
//let str_tailrec = 
//    [ for i in 1..100_000 -> TailRec.constant i]
//    |> TailRec.sequence
//let r_tailrec = str_tailrec |> TailRec.sample 1
//let str_tailrec2 = 
//    [ for i in 1..100_000 -> TailRec.constant i]
//    |> TailRec.sequence2
//let r_tailrec2 = str_tailrec2 |> TailRec.sample 1
//let str_tailrec_tramp = 
//    [ for i in 1..1 -> TailRecTrampoline.constant i]
//    |> TailRecTrampoline.sequence2
//let r_tailrec_tramp = str_tailrec_tramp |> TailRecTrampoline.sample 1
//TailRecTrampoline.apply (TailRecTrampoline.constant <| fun x -> x + 1) (TailRecTrampoline.constant 1) |> TailRecTrampoline.sample 1
//let str_nontailrec = 
//    [ for i in 1..1000 -> NonTailRec.constant i]
//    |> NonTailRec.sequence
//let r_nontailrec = str_nontailrec |> NonTailRec.sample 10_000


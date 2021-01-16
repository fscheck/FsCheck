namespace FsCheck.FSharp

[<AutoOpen>]
module GenBuilder =

    open FsCheck

    let private zero = Gen.constant ()

    let inline private tryFinally (Gen m) handler = 
        Gen (fun n r -> try m n r finally handler ())

    let inline private tryWith (Gen m) handler =
        Gen (fun n r -> try m n r with e -> let (Gen h) = handler e in h n r)

    let inline private dispose (x: #System.IDisposable) = x.Dispose()

    let inline private using r f = tryFinally (f r) (fun () -> dispose r)

    let inline private delay createGen : Gen<'T> = 
        Gen (fun n r -> let (Gen g) = createGen() in g n r)

    let inline private whileDo p (Gen m) =
        let go pred size rInit =
            let mutable r = rInit
            while pred() do 
                let struct (_,r1) = m size r
                r <- r1
            r            
        Gen (fun n r -> struct ((), go p n r))

    let inline private forDo (s:#seq<_>) f =
        using (s.GetEnumerator()) (fun ie ->
            whileDo (fun () -> ie.MoveNext()) (delay (fun () -> f ie.Current))
          )

    /// The computation expression type for generators.
    type GenBuilder internal() =
        member _.Zero() = zero
        member _.Return(a: 'T) : Gen<_> = Gen.constant a
        member _.ReturnFrom (g: Gen<'T>) = g
        member _.Delay(f: unit -> Gen<'T>) = delay f

        member _.Bind(m: Gen<'T>, k: 'T -> Gen<'U>) = Gen.bind k m
        member _.Combine(m1: Gen<unit>, m2: Gen<'T>) = Gen.bind (fun () -> m2) m1

        member _.TryFinally(m: Gen<'T>, handler: unit->unit) = tryFinally m handler
        member _.TryWith(m: Gen<'T>, handler) = tryWith m handler
        member _.Using (a: 'Disposable, k: 'Disposable -> Gen<'T>) =  using a k
        
        member _.While(p, m:Gen<'T>) = whileDo p m
        member _.For(s:#seq<'T>, f:('T -> Gen<'U>)) = forDo s f

        member _.MergeSources(gen1: Gen<'T1>, gen2: Gen<'T2>) = 
            Gen.zip gen1 gen2
        member _.MergeSources3(gen1: Gen<'T1>, gen2: Gen<'T2>, gen3: Gen<'T3>) =
            Gen.zip3 gen1 gen2 gen3

        member _.BindReturn(x: Gen<'T>, f: 'T -> 'U) = 
            Gen.map f x
        member _.Bind2Return(gen1: Gen<'T1>, gen2: Gen<'T2>, f: 'T1 * 'T2 -> 'T3) = 
            Gen.map2 (fun x y -> f(x,y)) gen1 gen2
        member _.Bind3Return(gen1: Gen<'T1>, gen2: Gen<'T2>, gen3: Gen<'T3>, f: 'T1 * 'T2 * 'T3 -> 'T4) =
            Gen.map3 (fun x y z -> f(x,y,z)) gen1 gen2 gen3

        member _.Bind2(gen1: Gen<'T1>, gen2: Gen<'T2>, f: 'T1 * 'T2 -> Gen<'T3>) = 
            Gen.zip gen1 gen2 |> Gen.bind f
        member _.Bind3(gen1: Gen<'T1>, gen2: Gen<'T2>, gen3: Gen<'T3>, f: 'T1 * 'T2 *'T3 -> Gen<'T4>) = 
            Gen.zip3 gen1 gen2 gen3 |> Gen.bind f
        

    /// The computation expressions for generators: gen { ... }
    let gen = GenBuilder()
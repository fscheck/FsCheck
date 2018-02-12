namespace FsCheck.Test

module Shrink =

    open Xunit
    open FsCheck
    open Swensen.Unquote

    // A disctinction is made between:
    // - immediate shrinks, which are all the values that are tried
    //  in order, provided none of the values tried are a good shrink.
    // - further shrinks, which are the values tried when each immediate shrink
    //  is a good shrink - i.e. the value still fails the test.

    // In a real test run, the actual shrinks will be a mixture of
    // immediate and good shrinks.


    [<Fact>]
    let ``empty should have no immediate shrinks``() =
        let result = 
            Shrink.empty<int>
            |> Shrink.toSeq
        test <@ Seq.isEmpty result @>

    [<Fact>]
    let ``empty should have no further shrinks``() =
        let result = 
            Shrink.empty<int>
            |> Shrink.shrink (fun _ -> true)

        test <@ Seq.isEmpty result @>
     
    [<Fact>]
    let ``ofShrinker immediate shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| 1..t  |])
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 1..5 |]

    [<Fact>]
    let ``ofShrinker further shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| t-1..(-1)..1 |])
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray
        result =! [| 4..(-1)..1 |]


    [<Fact>]
    let ``map immediate shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| 1..t |])
            |> Shrink.map ((*) 2)
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 2..2..10 |]

    [<Fact>]
    let ``map further shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| t-1..(-1)..1 |])
            |> Shrink.map ((*) 2)
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray
        result =! [| 8..(-2)..2 |]

    [<Fact>]
    let ``where immmediate shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| 1..t |])
            |> Shrink.where (fun i -> i <= 3)
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 1..3 |]

    [<Fact>]
    let ``where immediate shrinks that filters everythng is empty``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| 1..t |])
            |> Shrink.where (fun i -> i > 6)
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [||]

    [<Fact>]
    let ``where further shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| t-1..(-1)..1 |])
            |> Shrink.where (fun i -> i <= 3)
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray
        result =! [| 3..(-1)..1 |]

    [<Fact>]
    let ``apply immediate shrinks outer then inner``() =
        let f = Shrink.ofShrinker (fun e -> 1,e) (fun t -> upcast [| for i in 2..4 -> (fun e -> i,e) |])
        let a = Shrink.ofShrinker 3 (fun t -> { 1..t } )
        let result = 
            Shrink.apply (fun e -> 1,e) 3 f a
            |> Shrink.toSeq
            |> Seq.toArray
        // first three are shrinking f, then end up with * 4 and shrink a.
        result =! [| 2,3; 3,3; 4,3; 4,1; 4,2; 4,3 |]

    [<Fact>]
    let ``apply further shrinks outer then inner``() =
        let f = Shrink.ofShrinker 
                    (fun e -> 4,e) 
                    (fun t -> let (c,_) = t 1
                              seq { for i in (c-1)..(-1)..0 -> (fun e -> i,e)  })
        let a = Shrink.ofShrinker 3 (fun t -> { t-1..(-1)..1 } )
        let result = 
            Shrink.apply (fun e -> 4,e) 3 f a
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray
        // first three are shrinking f, then end up with * 4 and shrink a.
        result =! [|(3, 3); (2, 3); (1, 3); (0, 3); (0, 2); (0, 1)|]

    [<Fact>]
    let ``join immediate shrinks outer then inner``() =
        let a = Shrink.ofShrinker 6 (fun t -> { t..(-1)..3  })
                |> Shrink.map (fun t ->  Shrink.ofShrinker (t,t) (fun (t,t') -> seq { for i in t'..(-1)..0 -> t,i}))
        let result = 
            Shrink.join Shrink.empty a
            |> Shrink.toSeq
            |> Seq.toArray
        
        result =! [|(6, 6); (5, 5); (4, 4); (3, 3); (3, 2); (3, 1); (3, 0)|] 
        

    [<Fact>]
    let ``join further shrinks outer then inner``() =
        let a = Shrink.ofShrinker 6 (fun t -> { t-1..(-1)..3  })
                |> Shrink.map (fun t ->  Shrink.ofShrinker (t,t) (fun (t,t') -> seq { for i in t'-1..(-1)..0 -> t,i}))
        let result = 
            Shrink.join Shrink.empty a
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray
        
        result =! [|(5, 4); (4, 3); (3, 2); (3, 1); (3, 0)|] 

    [<Fact>]
    let ``elements immediate shrinks array elements one by one``() =
        let initial = [| 1..5 |]
        let shrinkers = initial |> Array.map (fun i -> Shrink.ofShrinker i (fun i -> { i-1..(-1)..0 }))
        let result =
            Shrink.elements initial shrinkers
            |> Shrink.toSeq
            |> Seq.toArray

        result =! [| [|0; 2; 3; 4; 5|]
                     [|1; 1; 3; 4; 5|]; [|1; 0; 3; 4; 5|]
                     [|1; 2; 2; 4; 5|]; [|1; 2; 1; 4; 5|]; [|1; 2; 0; 4; 5|]; 
                     [|1; 2; 3; 3; 5|]; [|1; 2; 3; 2; 5|]; [|1; 2; 3; 1; 5|]; [|1; 2; 3; 0; 5|]; 
                     [|1; 2; 3; 4; 4|]; [|1; 2; 3; 4; 3|]; [|1; 2; 3; 4; 2|]; [|1; 2; 3; 4; 1|]; [|1; 2; 3; 4; 0|]|]


    [<Fact>]
    let ``elements further hrinks array elements one by one``() =
        let initial = [| 1..5 |]
        let shrinkers = initial |> Array.map (fun i -> Shrink.ofShrinker i (fun i -> { i-1..(-1)..0 }))
        let result =
            Shrink.elements initial shrinkers
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray

        result =! [| [|0; 2; 3; 4; 5|]
                     [|0; 1; 3; 4; 5|]; [|0; 0; 3; 4; 5|]
                     [|0; 0; 2; 4; 5|]; [|0; 0; 1; 4; 5|]; [|0; 0; 0; 4; 5|]; 
                     [|0; 0; 0; 3; 5|]; [|0; 0; 0; 2; 5|]; [|0; 0; 0; 1; 5|]; [|0; 0; 0; 0; 5|]; 
                     [|0; 0; 0; 0; 4|]; [|0; 0; 0; 0; 3|]; [|0; 0; 0; 0; 2|]; [|0; 0; 0; 0; 1|]; [|0; 0; 0; 0; 0|]|]

    [<Fact>]
    let ``array immediate shrinks by removing array elements one by one``() =
        let initial = [| 1..5 |]
        let result =
            Shrink.array initial
            |> Shrink.toSeq
            |> Seq.toArray

        result =! [|[|2; 3; 4; 5|]; [|1; 3; 4; 5|]; [|1; 2; 4; 5|]; [|1; 2; 3; 5|]; [|1; 2; 3; 4|]|]

    [<Fact>]
    let ``array getShrinks by removing array elements one by one``() =
        let initial = [| 1..5 |]
        let result =
            Shrink.array initial
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray

        result =! [|[|2; 3; 4; 5|]; [| 3; 4; 5|]; [|4; 5|]; [| 5 |]; [||] |]
    
    [<Fact>]
    let ``append immediate shrinks from first then second stream``() =
        let shrink1 = Shrink.ofShrinker 10 (fun i -> { i-1..(-1)..5 })
        let shrink2 = Shrink.ofShrinker 5 (fun i -> { i-1..(-1)..0 })
        let result =
            Shrink.append shrink1 shrink2
            |> Shrink.toSeq
            |> Seq.toArray

        result =! [| 9..(-1)..0 |]

    [<Fact>]
    let ``append further shrinks from first then second stream``() =
        let shrink1 = Shrink.ofShrinker 10 (fun i -> { i-1..(-1)..5 })
        let shrink2 = Shrink.ofShrinker 5 (fun i -> { i-1..(-1)..0 })
        let result =
            Shrink.append shrink1 shrink2
            |> Shrink.shrink (fun _ -> true)
            |> Seq.toArray

        result =! [| 9..(-1)..0 |]

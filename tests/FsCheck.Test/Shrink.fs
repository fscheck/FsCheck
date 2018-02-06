namespace FsCheck.Test

module Shrink =

    open Xunit
    open FsCheck
    open Swensen.Unquote

    [<Fact>]
    let ``empty should have no shrinks``() =
        let result = 
            Shrink.empty<int>
            |> Shrink.toSeq
        test <@ Seq.isEmpty result @>

    [<Fact>]
    let ``empty should have empty getShrinks``() =
        let result = 
            Shrink.empty<int>
            |> Shrink.getShrinks
            |> Shrink.toSeq
        test <@ Seq.isEmpty result @>
     
    [<Fact>]
    let ``ofShrinker shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| for i in 1..t -> i |])
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 1..5 |]

    [<Fact>]
    let ``ofShrinker getShrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| for i in 1..t -> i |])
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 1..5 |]


    [<Fact>]
    let ``map shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| for i in 1..t -> i |])
            |> Shrink.map ((*) 2)
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 2..2..10 |]

    [<Fact>]
    let ``map getShrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| for i in 1..t -> i |])
            |> Shrink.map ((*) 2)
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 2..2..10 |]

    [<Fact>]
    let ``where shrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| for i in 1..t -> i |])
            |> Shrink.where (fun i -> i <= 3)
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 1..3 |]

    [<Fact>]
    let ``empty where shrinks is empty``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| for i in 1..t -> i |])
            |> Shrink.where (fun i -> i > 6)
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [||]

    [<Fact>]
    let ``where getShrinks``() =
        let result =
            Shrink.ofShrinker 5 (fun t -> upcast [| for i in 1..t -> i |])
            |> Shrink.where (fun i -> i <= 3)
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray
        result =! [| 1..3 |]

    [<Fact>]
    let ``apply shrinks outer then inner``() =
        let f = Shrink.ofShrinker (fun e -> 1,e) (fun t -> upcast [| for i in 2..4 -> (fun e -> i,e) |])
        let a = Shrink.ofShrinker 3 (fun t -> upcast [| for i in 1..t -> i |])
        let result = 
            Shrink.apply (fun e -> 1,e) 3 f a
            |> Shrink.toSeq
            |> Seq.toArray
        // first three are shrinking f, then end up with * 4 and shrink a.
        result =! [| 2,3; 3,3; 4,3; 4,1; 4,2; 4,3 |]

    [<Fact>]
    let ``apply getShrinks outer then inner``() =
        let f = Shrink.ofShrinker (fun e -> 1,e) (fun t -> upcast [| for i in 2..4 -> (fun e -> i,e) |])
        let a = Shrink.ofShrinker 3 (fun t -> upcast [| for i in 1..t -> i |])
        let result = 
            Shrink.apply (fun e -> 1,e) 3 f a
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray
        // first three are shrinking f, then end up with * 4 and shrink a.
        result =! [| 2,3; 3,3; 4,3; 4,1; 4,2; 4,3 |]

    [<Fact>]
    let ``join shrinks outer then inner``() =
        let a = Shrink.ofShrinker 6 (fun t -> { t..(-1)..3  })
                |> Shrink.map (fun t ->  Shrink.ofShrinker (t,t) (fun (t,t') -> seq { for i in t'..(-1)..0 -> t,i}))
        let result = 
            Shrink.join Shrink.empty a
            |> Shrink.toSeq
            |> Seq.toArray
        
        result =! [|(6, 6); (5, 5); (4, 4); (3, 3); (3, 2); (3, 1); (3, 0)|] 
        

    [<Fact>]
    let ``join getShrinks outer then inner``() =
        let a = Shrink.ofShrinker 6 (fun t -> { t..(-1)..3  })
                |> Shrink.map (fun t ->  Shrink.ofShrinker (t,t) (fun (t,t') -> seq { for i in t'..(-1)..0 -> t,i}))
        let result = 
            Shrink.join Shrink.empty a
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray
        
        result =! [|(6, 6); (5, 5); (4, 4); (3, 3); (3, 2); (3, 1); (3, 0)|] 

    [<Fact>]
    let ``elements shrinks array elements one by one``() =
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
    let ``elements getShrinks array elements one by one``() =
        let initial = [| 1..5 |]
        let shrinkers = initial |> Array.map (fun i -> Shrink.ofShrinker i (fun i -> { i-1..(-1)..0 }))
        let result =
            Shrink.elements initial shrinkers
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray

        result =! [| [|0; 2; 3; 4; 5|]
                     [|1; 1; 3; 4; 5|]; [|1; 0; 3; 4; 5|]
                     [|1; 2; 2; 4; 5|]; [|1; 2; 1; 4; 5|]; [|1; 2; 0; 4; 5|]; 
                     [|1; 2; 3; 3; 5|]; [|1; 2; 3; 2; 5|]; [|1; 2; 3; 1; 5|]; [|1; 2; 3; 0; 5|]; 
                     [|1; 2; 3; 4; 4|]; [|1; 2; 3; 4; 3|]; [|1; 2; 3; 4; 2|]; [|1; 2; 3; 4; 1|]; [|1; 2; 3; 4; 0|]|]

    [<Fact>]
    let ``array shrinks by removing array elements one by one``() =
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
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray

        result =! [|[|2; 3; 4; 5|]; [|1; 3; 4; 5|]; [|1; 2; 4; 5|]; [|1; 2; 3; 5|]; [|1; 2; 3; 4|]|]
    
    [<Fact>]
    let ``append shrinks from first then second stream``() =
        let shrink1 = Shrink.ofShrinker 10 (fun i -> { i-1..(-1)..5 })
        let shrink2 = Shrink.ofShrinker 5 (fun i -> { i-1..(-1)..0 })
        let result =
            Shrink.append shrink1 shrink2
            |> Shrink.toSeq
            |> Seq.toArray

        result =! [| 9..(-1)..0 |]

    [<Fact>]
    let ``append getShrinks from first then second stream``() =
        let shrink1 = Shrink.ofShrinker 10 (fun i -> { i-1..(-1)..5 })
        let shrink2 = Shrink.ofShrinker 5 (fun i -> { i-1..(-1)..0 })
        let result =
            Shrink.append shrink1 shrink2
            |> Shrink.getShrinks
            |> Shrink.toSeq
            |> Seq.toArray

        result =! [| 9..(-1)..0 |]
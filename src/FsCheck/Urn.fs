namespace FsCheck

type Weight = int32
type Size = uint32
type Index = int32

type internal Tree<'a> = 
    | Node of Weight * Tree<'a> * Tree<'a>
    | Leaf of Weight * 'a

type internal Urn<'a> = { Size: Size; Tree: Tree<'a> }

module internal Urn =
    
    let private weightT tree =
        match tree with
        | Node (w,_,_)
        | Leaf (w,_) -> w

    /// Return the sum of all the weights in the Urn.
    /// This is the strict upper bound for the index.
    let totalWeight urn =
        weightT urn.Tree

    /// Sample the element at index i.
    let sample urn (i:Index) =
        let rec sampleT tree i =
            match tree with
            | Leaf (_, a) -> a
            | Node(_, l, r) -> 
                let wl = weightT l
                if i < wl then 
                    sampleT l i
                else 
                    sampleT r (i-wl)
        sampleT urn.Tree i

    /// Update the value at index using the update function.
    /// Returns the old weighted value, the new weighted value,
    /// and the new Urn.
    let update (i:Index) upd urn =
        let rec updateT tree i =
            match tree with
            | Leaf (w,a) ->
                let wa' = upd (w,a)
                (w,a), wa', Leaf wa'
            | Node (w,l,r) ->
                let wl = weightT l
                if i < wl then
                    let (old, newV, l') = updateT l i
                    old, newV, Node (w - fst old + fst newV, l', r)
                else
                    let (old, newV, r') = updateT r (i-wl)
                    old, newV, Node (w - fst old + fst newV, l, r')
        let (oldV, newV, tree) = updateT urn.Tree i
        oldV, newV, { urn with Tree = tree }

    /// Replace the value at index with the given weighted value.
    /// Returns the old weighted value and the new Urn.
    let replace (i:Index) weightedValue urn =  
        let (o,_,u) = update i (fun _ -> weightedValue) urn
        (o,u)

    /// Insert a new weighted value into the Urn, 
    /// increasing the size with 1 and the weight with weight.
    let insert (weight,value) { Size = size; Tree = tree } =
        let rec go path tree =
            match tree with
            | Leaf (w,_) as l -> 
                Node (w+weight, l, Leaf (weight,value))
            | Node (w, l, r) ->
                let path' = path >>> 1
                if path &&& 1u = 1u then //right
                    Node (w+weight, l, go path' r)
                else                     //left
                    Node (w+weight, go path' l, r)
        { Size = size + 1u; Tree = go size tree }

    let private uninsert { Size = size; Tree = tree } =
        let rec go path tree =
            match tree with
            | Leaf (w,a) -> 
                w, a , 0, None
            | Node (w, l, r) ->
                let path' = path >>> 1
                if path &&& 1u = 1u then
                    let (w',a', lb, t) = go path' r
                    match t with
                    | Some r' -> w', a', lb + weightT l, Some (Node (w-w', l, r'))
                    | None    -> w', a', lb + weightT l, Some l
                else
                    let (w',a', lb, t) = go path' l
                    match t with
                    | Some l' -> w', a', lb, Some (Node (w-w', l', r))
                    | None    -> w', a', lb, Some r
        let (w', a', lb, mt) = go size tree
        (w', a'), lb, mt |> Option.map (fun t -> { Size = size - 1u; Tree = t})

    /// Remove the weighted value at index i from the Urn.
    /// Return the removed value, and the new Urn if the removal
    /// does not make it empty.
    let remove (i:Index) urn =
        let ((w,_) as wa, lb, maybeUrn) = uninsert urn
        match maybeUrn with
        | None -> wa, None
        | Some urn' ->
            if i < lb then
                let (wa,urn) = replace i wa urn'
                wa, Some urn
            elif i < lb + w then
                wa, Some urn'
            else
                let (wa, urn) = replace (i - w) wa urn' 
                wa, Some urn

    /// Create an Urn with a single element.
    let singleton wa =
        { Size = 1u; Tree = Leaf wa }
        
    /// Create an Urn of a sequence of weighted elements.
    let ofSeq s =
        let wa = Seq.head s
        let was = s |> Seq.skip 1
        was |> Seq.fold (fun st e -> insert e st) (singleton wa)


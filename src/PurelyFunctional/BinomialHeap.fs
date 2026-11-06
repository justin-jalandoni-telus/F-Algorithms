namespace PurelyFunctional

/// Okasaki's binomial heap (Chapter 3): a list of heap-ordered binomial trees
/// of strictly increasing rank, mirroring binary addition for O(log n) merge.
module BinomialHeap =

    /// A binomial tree of a given rank: a root value and its child trees in
    /// decreasing rank order.
    type Tree<'T> = Node of int * 'T * Tree<'T> list

    /// A binomial heap: trees in strictly increasing rank order.
    type Heap<'T> = Tree<'T> list

    /// The empty heap.
    let empty<'T> : Heap<'T> = []

    /// Whether the heap is empty.
    let isEmpty (h: Heap<'T>) = List.isEmpty h

    let private rank (Node(r, _, _)) = r
    let private root (Node(_, x, _)) = x

    let private link (Node(r, x1, c1) as t1) (Node(_, x2, c2) as t2) =
        if x1 <= x2 then Node(r + 1, x1, t2 :: c1)
        else Node(r + 1, x2, t1 :: c2)

    let rec private insTree t (ts: Heap<'T>) : Heap<'T> =
        match ts with
        | [] -> [ t ]
        | t' :: rest ->
            if rank t < rank t' then t :: ts
            else insTree (link t t') rest

    /// Inserts a value.
    let insert x (h: Heap<'T>) : Heap<'T> = insTree (Node(0, x, [])) h

    /// Merges two heaps.
    let rec merge (h1: Heap<'T>) (h2: Heap<'T>) : Heap<'T> =
        match h1, h2 with
        | _, [] -> h1
        | [], _ -> h2
        | t1 :: ts1, t2 :: ts2 ->
            if rank t1 < rank t2 then t1 :: merge ts1 h2
            elif rank t2 < rank t1 then t2 :: merge h1 ts2
            else insTree (link t1 t2) (merge ts1 ts2)

    let rec private removeMinTree (ts: Heap<'T>) =
        match ts with
        | [] -> failwith "removeMinTree: empty heap"
        | [ t ] -> t, []
        | t :: rest ->
            let t', rest' = removeMinTree rest
            if root t <= root t' then t, rest else t', t :: rest'

    /// The minimum value.
    let findMin (h: Heap<'T>) =
        let t, _ = removeMinTree h
        root t

    /// The heap without its minimum value.
    let deleteMin (h: Heap<'T>) : Heap<'T> =
        let Node(_, _, children), rest = removeMinTree h
        merge (List.rev children) rest

    /// Builds a heap from a sequence of values.
    let ofSeq items : Heap<'T> = items |> Seq.fold (fun h x -> insert x h) empty

    /// Drains the heap into an ascending list.
    let rec toSortedList (h: Heap<'T>) =
        if isEmpty h then []
        else findMin h :: toSortedList (deleteMin h)

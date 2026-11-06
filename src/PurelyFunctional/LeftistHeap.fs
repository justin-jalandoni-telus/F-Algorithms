namespace PurelyFunctional

/// Okasaki's leftist heap (Chapter 3): a heap-ordered binary tree whose right
/// spine (the "rank") is always the shortest, so merge is O(log n).
module LeftistHeap =

    /// A leftist-heap node caching its rank, or <c>Empty</c>.
    type Heap<'T> =
        | Empty
        | Node of int * 'T * Heap<'T> * Heap<'T>

    /// The empty heap.
    let empty<'T> : Heap<'T> = Empty

    /// Whether the heap is empty.
    let isEmpty h =
        match h with
        | Empty -> true
        | _ -> false

    let private rank h =
        match h with
        | Empty -> 0
        | Node(r, _, _, _) -> r

    let private makeNode x a b =
        if rank a >= rank b then Node(rank b + 1, x, a, b)
        else Node(rank a + 1, x, b, a)

    /// Merges two heaps.
    let rec merge h1 h2 =
        match h1, h2 with
        | h, Empty
        | Empty, h -> h
        | Node(_, x, a1, b1), Node(_, y, a2, b2) ->
            if x <= y then makeNode x a1 (merge b1 h2)
            else makeNode y a2 (merge h1 b2)

    /// Inserts a value.
    let insert x h = merge (Node(1, x, Empty, Empty)) h

    /// The minimum value.
    let findMin h =
        match h with
        | Empty -> failwith "findMin: empty heap"
        | Node(_, x, _, _) -> x

    /// The heap without its minimum value.
    let deleteMin h =
        match h with
        | Empty -> failwith "deleteMin: empty heap"
        | Node(_, _, a, b) -> merge a b

    /// Builds a heap from a sequence of values.
    let ofSeq items : Heap<'T> = items |> Seq.fold (fun h x -> insert x h) empty

    /// Drains the heap into an ascending list.
    let rec toSortedList h =
        if isEmpty h then []
        else findMin h :: toSortedList (deleteMin h)

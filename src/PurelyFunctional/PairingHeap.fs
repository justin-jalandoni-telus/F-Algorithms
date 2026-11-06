namespace PurelyFunctional

/// Okasaki's pairing heap (Chapter 5): a multi-way heap-ordered tree that is
/// trivial to define yet performs excellently in practice.
module PairingHeap =

    /// A pairing-heap node (a root and its child heaps), or <c>Empty</c>.
    type Heap<'T> =
        | Empty
        | Node of 'T * Heap<'T> list

    /// The empty heap.
    let empty<'T> : Heap<'T> = Empty

    /// Whether the heap is empty.
    let isEmpty h =
        match h with
        | Empty -> true
        | _ -> false

    /// Merges two heaps.
    let merge h1 h2 =
        match h1, h2 with
        | h, Empty
        | Empty, h -> h
        | Node(x, hs1), Node(y, hs2) ->
            if x <= y then Node(x, h2 :: hs1) else Node(y, h1 :: hs2)

    /// Inserts a value.
    let insert x h = merge (Node(x, [])) h

    /// The minimum value.
    let findMin h =
        match h with
        | Empty -> failwith "findMin: empty heap"
        | Node(x, _) -> x

    let rec private mergePairs hs =
        match hs with
        | [] -> Empty
        | [ h ] -> h
        | h1 :: h2 :: rest -> merge (merge h1 h2) (mergePairs rest)

    /// The heap without its minimum value.
    let deleteMin h =
        match h with
        | Empty -> failwith "deleteMin: empty heap"
        | Node(_, hs) -> mergePairs hs

    /// Builds a heap from a sequence of values.
    let ofSeq items : Heap<'T> = items |> Seq.fold (fun h x -> insert x h) empty

    /// Drains the heap into an ascending list.
    let rec toSortedList h =
        if isEmpty h then []
        else findMin h :: toSortedList (deleteMin h)

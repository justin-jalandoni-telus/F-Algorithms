namespace PurelyFunctional

/// Okasaki's splay heap (Chapter 5): a self-adjusting binary search tree used
/// as a heap. <c>partition</c> splays around an inserted pivot.
module SplayHeap =

    /// A splay-tree node, or <c>Empty</c>.
    type Heap<'T> =
        | Empty
        | Node of Heap<'T> * 'T * Heap<'T>

    /// The empty heap.
    let empty<'T> : Heap<'T> = Empty

    /// Whether the heap is empty.
    let isEmpty h =
        match h with
        | Empty -> true
        | _ -> false

    /// Splits a tree into the elements <= pivot and the elements > pivot,
    /// splaying as it descends.
    let rec private partition pivot t =
        match t with
        | Empty -> Empty, Empty
        | Node(a, x, b) ->
            if x <= pivot then
                match b with
                | Empty -> t, Empty
                | Node(b1, y, b2) ->
                    if y <= pivot then
                        let small, big = partition pivot b2
                        Node(Node(a, x, b1), y, small), big
                    else
                        let small, big = partition pivot b1
                        Node(a, x, small), Node(big, y, b2)
            else
                match a with
                | Empty -> Empty, t
                | Node(a1, y, a2) ->
                    if y <= pivot then
                        let small, big = partition pivot a2
                        Node(a1, y, small), Node(big, x, b)
                    else
                        let small, big = partition pivot a1
                        small, Node(big, y, Node(a2, x, b))

    /// Inserts a value.
    let insert x t =
        let a, b = partition x t
        Node(a, x, b)

    /// Merges two heaps.
    let rec merge h1 h2 =
        match h1 with
        | Empty -> h2
        | Node(a, x, b) ->
            let ta, tb = partition x h2
            Node(merge ta a, x, merge tb b)

    /// The minimum value.
    let rec findMin h =
        match h with
        | Empty -> failwith "findMin: empty heap"
        | Node(Empty, x, _) -> x
        | Node(a, _, _) -> findMin a

    /// The heap without its minimum value.
    let rec deleteMin h =
        match h with
        | Empty -> failwith "deleteMin: empty heap"
        | Node(Empty, _, b) -> b
        | Node(Node(Empty, _, b), y, c) -> Node(b, y, c)
        | Node(Node(a, x, b), y, c) -> Node(deleteMin (Node(a, x, b)), y, c)

    /// Builds a heap from a sequence of values.
    let ofSeq items : Heap<'T> = items |> Seq.fold (fun h x -> insert x h) empty

    /// Drains the heap into an ascending list.
    let rec toSortedList h =
        if isEmpty h then []
        else findMin h :: toSortedList (deleteMin h)

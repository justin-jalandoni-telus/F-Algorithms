namespace PurelyFunctional

/// Okasaki's skew binary random-access list (Chapter 9): a skew-binary
/// numerical representation giving worst-case O(1) cons/head/tail while keeping
/// O(log n) lookup/update.
module SkewBinaryRandomAccessList =

    /// A complete binary tree; sizes are tracked alongside roots in the list.
    type Tree<'T> =
        | Leaf of 'T
        | Node of 'T * Tree<'T> * Tree<'T>

    /// A random-access list as size-tagged trees in non-decreasing size order
    /// (with at most two trees of the smallest size).
    type RList<'T> = (int * Tree<'T>) list

    /// The empty list.
    let empty<'T> : RList<'T> = []

    /// Whether the list is empty.
    let isEmpty (ts: RList<'T>) = List.isEmpty ts

    /// Prepends a value in O(1).
    let cons x (ts: RList<'T>) : RList<'T> =
        match ts with
        | (w1, t1) :: (w2, t2) :: rest when w1 = w2 -> (1 + w1 + w2, Node(x, t1, t2)) :: rest
        | _ -> (1, Leaf x) :: ts

    /// The first element.
    let head (ts: RList<'T>) =
        match ts with
        | [] -> failwith "head: empty list"
        | (_, Leaf x) :: _ -> x
        | (_, Node(x, _, _)) :: _ -> x

    /// The list without its first element.
    let tail (ts: RList<'T>) : RList<'T> =
        match ts with
        | [] -> failwith "tail: empty list"
        | (_, Leaf _) :: rest -> rest
        | (w, Node(_, t1, t2)) :: rest -> (w / 2, t1) :: (w / 2, t2) :: rest

    let rec private lookupTree w i t =
        match i, t with
        | 0, Leaf x -> x
        | _, Leaf _ -> raise (System.IndexOutOfRangeException())
        | 0, Node(x, _, _) -> x
        | _, Node(_, t1, t2) ->
            if i <= w / 2 then lookupTree (w / 2) (i - 1) t1
            else lookupTree (w / 2) (i - 1 - w / 2) t2

    /// The element at index <paramref name="i"/> (0-based).
    let rec lookup i (ts: RList<'T>) =
        match ts with
        | [] -> raise (System.IndexOutOfRangeException())
        | (w, t) :: rest -> if i < w then lookupTree w i t else lookup (i - w) rest

    let rec private updateTree (w, i) y t =
        match i, t with
        | 0, Leaf _ -> Leaf y
        | _, Leaf _ -> raise (System.IndexOutOfRangeException())
        | 0, Node(_, t1, t2) -> Node(y, t1, t2)
        | _, Node(x, t1, t2) ->
            if i <= w / 2 then Node(x, updateTree (w / 2, i - 1) y t1, t2)
            else Node(x, t1, updateTree (w / 2, i - 1 - w / 2) y t2)

    /// A copy with index <paramref name="i"/> set to <paramref name="y"/>.
    let rec update i y (ts: RList<'T>) : RList<'T> =
        match ts with
        | [] -> raise (System.IndexOutOfRangeException())
        | (w, t) :: rest ->
            if i < w then (w, updateTree (w, i) y t) :: rest
            else (w, t) :: update (i - w) y rest

    /// The number of elements.
    let length (ts: RList<'T>) = ts |> List.sumBy fst

    /// Builds a list from a sequence (first element at index 0).
    let ofList xs : RList<'T> = List.foldBack cons xs empty

    /// Materializes the list in index order.
    let toList (ts: RList<'T>) = [ for i in 0 .. length ts - 1 -> lookup i ts ]

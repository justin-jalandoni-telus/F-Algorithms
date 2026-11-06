namespace PurelyFunctional

/// Okasaki's binary random-access list (Chapter 9): a numerical representation
/// where the list is a sequence of binary digits, each 'one' holding a complete
/// binary tree. Gives O(log n) cons/head/tail and O(log n) lookup/update.
module BinaryRandomAccessList =

    /// A complete binary tree caching its size at internal nodes.
    type Tree<'T> =
        | Leaf of 'T
        | Node of int * Tree<'T> * Tree<'T>

    /// A binary digit: absent (<c>Zero</c>) or a tree of the digit's rank.
    type Digit<'T> =
        | Zero
        | One of Tree<'T>

    /// A random-access list as a little-endian list of digits.
    type RList<'T> = Digit<'T> list

    /// The empty list.
    let empty<'T> : RList<'T> = []

    /// Whether the list is empty.
    let isEmpty (ts: RList<'T>) = List.isEmpty ts

    let private size t =
        match t with
        | Leaf _ -> 1
        | Node(w, _, _) -> w

    let private link t1 t2 = Node(size t1 + size t2, t1, t2)

    let rec private consTree t (ts: Digit<'T> list) =
        match ts with
        | [] -> [ One t ]
        | Zero :: rest -> One t :: rest
        | One t' :: rest -> Zero :: consTree (link t t') rest

    let rec private unconsTree (ts: Digit<'T> list) =
        match ts with
        | [] -> failwith "unconsTree: empty list"
        | One t :: rest ->
            match rest with
            | [] -> t, []
            | _ -> t, Zero :: rest
        | Zero :: rest ->
            match unconsTree rest with
            | Node(_, t1, t2), rest' -> t1, One t2 :: rest'
            | Leaf _, _ -> failwith "unconsTree: malformed list"

    /// Prepends a value.
    let cons x (ts: RList<'T>) : RList<'T> = consTree (Leaf x) ts

    /// The first element.
    let head (ts: RList<'T>) =
        match unconsTree ts with
        | Leaf x, _ -> x
        | Node _, _ -> failwith "head: malformed list"

    /// The list without its first element.
    let tail (ts: RList<'T>) : RList<'T> =
        let _, rest = unconsTree ts
        rest

    let rec private lookupTree i t =
        match t with
        | Leaf x -> if i = 0 then x else raise (System.IndexOutOfRangeException())
        | Node(w, t1, t2) ->
            if i < w / 2 then lookupTree i t1 else lookupTree (i - w / 2) t2

    /// The element at index <paramref name="i"/> (0-based).
    let rec lookup i (ts: RList<'T>) =
        match ts with
        | [] -> raise (System.IndexOutOfRangeException())
        | Zero :: rest -> lookup i rest
        | One t :: rest -> if i < size t then lookupTree i t else lookup (i - size t) rest

    let rec private updateTree i y t =
        match t with
        | Leaf _ -> if i = 0 then Leaf y else raise (System.IndexOutOfRangeException())
        | Node(w, t1, t2) ->
            if i < w / 2 then Node(w, updateTree i y t1, t2)
            else Node(w, t1, updateTree (i - w / 2) y t2)

    /// A copy with index <paramref name="i"/> set to <paramref name="y"/>.
    let rec update i y (ts: RList<'T>) : RList<'T> =
        match ts with
        | [] -> raise (System.IndexOutOfRangeException())
        | Zero :: rest -> Zero :: update i y rest
        | One t :: rest ->
            if i < size t then One(updateTree i y t) :: rest
            else One t :: update (i - size t) y rest

    /// The number of elements.
    let length (ts: RList<'T>) =
        ts
        |> List.sumBy (fun d ->
            match d with
            | Zero -> 0
            | One t -> size t)

    /// Builds a list from a sequence (first element at index 0).
    let ofList xs : RList<'T> = List.foldBack cons xs empty

    /// Materializes the list in index order.
    let toList (ts: RList<'T>) = [ for i in 0 .. length ts - 1 -> lookup i ts ]

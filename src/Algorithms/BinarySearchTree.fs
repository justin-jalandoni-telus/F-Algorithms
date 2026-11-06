namespace Algorithms

/// Ordered-set operations that maintain the binary-search-tree invariant over
/// <see cref="BinaryTree.Tree{T}"/>. This variant does not self-balance; use
/// <see cref="AvlTree"/> or <see cref="RedBlackTree"/> when balance matters.
module Bst =

    /// The empty search tree.
    let empty<'T> : BinaryTree.Tree<'T> = BinaryTree.empty

    /// Inserts a value, ignoring duplicates.
    let rec insert (value: 'T) (tree: BinaryTree.Tree<'T>) : BinaryTree.Tree<'T> =
        match tree with
        | BinaryTree.Empty -> BinaryTree.leaf value
        | BinaryTree.Node(l, v, r) ->
            if value < v then BinaryTree.Node(insert value l, v, r)
            elif value > v then BinaryTree.Node(l, v, insert value r)
            else tree

    /// Whether the value is present.
    let rec contains value tree =
        match tree with
        | BinaryTree.Empty -> false
        | BinaryTree.Node(l, v, r) ->
            if value < v then contains value l
            elif value > v then contains value r
            else true

    /// The smallest value, if any.
    let rec tryMin tree =
        match tree with
        | BinaryTree.Empty -> None
        | BinaryTree.Node(BinaryTree.Empty, v, _) -> Some v
        | BinaryTree.Node(l, _, _) -> tryMin l

    /// The largest value, if any.
    let rec tryMax tree =
        match tree with
        | BinaryTree.Empty -> None
        | BinaryTree.Node(_, v, BinaryTree.Empty) -> Some v
        | BinaryTree.Node(_, _, r) -> tryMax r

    /// Removes a value, replacing an interior node with its in-order successor.
    let rec remove value tree =
        match tree with
        | BinaryTree.Empty -> BinaryTree.Empty
        | BinaryTree.Node(l, v, r) ->
            if value < v then BinaryTree.Node(remove value l, v, r)
            elif value > v then BinaryTree.Node(l, v, remove value r)
            else
                match l, r with
                | BinaryTree.Empty, _ -> r
                | _, BinaryTree.Empty -> l
                | _ ->
                    match tryMin r with
                    | Some successor -> BinaryTree.Node(l, successor, remove successor r)
                    | None -> l

    /// Builds a search tree from a sequence of values.
    let ofSeq items : BinaryTree.Tree<'T> =
        items |> Seq.fold (fun tree x -> insert x tree) empty

    /// In-order (ascending) enumeration.
    let toList tree = BinaryTree.toList tree

    /// The number of values stored.
    let count tree = BinaryTree.size tree

    /// The height of the underlying tree.
    let height tree = BinaryTree.height tree

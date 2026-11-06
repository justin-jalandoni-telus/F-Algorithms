namespace Algorithms

/// An immutable self-balancing AVL tree. Each node caches its height so that
/// balancing is O(1) per rebalance step and lookups stay O(log n).
module AvlTree =

    /// An AVL node caching its own height, or <c>Empty</c>.
    type Tree<'T> =
        | Empty
        | Node of Tree<'T> * 'T * Tree<'T> * int

    /// The empty tree.
    let empty<'T> : Tree<'T> = Empty

    /// The cached height of a tree (0 for empty).
    let height tree =
        match tree with
        | Empty -> 0
        | Node(_, _, _, h) -> h

    let private make left value right =
        Node(left, value, right, 1 + max (height left) (height right))

    let private balanceFactor tree =
        match tree with
        | Empty -> 0
        | Node(l, _, r, _) -> height l - height r

    let private rotateRight tree =
        match tree with
        | Node(Node(ll, lv, lr, _), v, r, _) -> make ll lv (make lr v r)
        | _ -> tree

    let private rotateLeft tree =
        match tree with
        | Node(l, v, Node(rl, rv, rr, _), _) -> make (make l v rl) rv rr
        | _ -> tree

    let private balance left value right =
        let factor = height left - height right
        if factor > 1 then
            let left' = if balanceFactor left < 0 then rotateLeft left else left
            rotateRight (make left' value right)
        elif factor < -1 then
            let right' = if balanceFactor right > 0 then rotateRight right else right
            rotateLeft (make left value right')
        else
            make left value right

    /// Inserts a value, rebalancing on the way up. Duplicates are ignored.
    let rec insert value tree =
        match tree with
        | Empty -> make Empty value Empty
        | Node(l, v, r, _) ->
            if value < v then balance (insert value l) v r
            elif value > v then balance l v (insert value r)
            else tree

    let rec private removeMin tree =
        match tree with
        | Empty -> failwith "removeMin: empty tree"
        | Node(Empty, v, r, _) -> v, r
        | Node(l, v, r, _) ->
            let minValue, l' = removeMin l
            minValue, balance l' v r

    /// Removes a value, rebalancing on the way up.
    let rec remove value tree =
        match tree with
        | Empty -> Empty
        | Node(l, v, r, _) ->
            if value < v then balance (remove value l) v r
            elif value > v then balance l v (remove value r)
            else
                match l, r with
                | Empty, _ -> r
                | _, Empty -> l
                | _ ->
                    let successor, r' = removeMin r
                    balance l successor r'

    /// Whether the value is present.
    let rec contains value tree =
        match tree with
        | Empty -> false
        | Node(l, v, r, _) ->
            if value < v then contains value l
            elif value > v then contains value r
            else true

    /// In-order (ascending) enumeration.
    let toList tree =
        let rec loop acc t =
            match t with
            | Empty -> acc
            | Node(l, v, r, _) -> loop (v :: loop acc r) l
        loop [] tree

    /// Builds a balanced tree from a sequence of values.
    let ofSeq items : Tree<'T> =
        items |> Seq.fold (fun tree x -> insert x tree) empty

    /// The number of values stored.
    let rec count tree =
        match tree with
        | Empty -> 0
        | Node(l, _, r, _) -> 1 + count l + count r

    /// Whether every node satisfies the AVL balance invariant.
    let rec isBalanced tree =
        match tree with
        | Empty -> true
        | Node(l, _, r, _) -> abs (height l - height r) <= 1 && isBalanced l && isBalanced r

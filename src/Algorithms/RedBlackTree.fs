namespace Algorithms

/// An immutable red-black tree (balanced BST) built with Okasaki's functional
/// insertion. The elegant <c>balance</c> step restores the invariants after
/// each insert using a single pattern match over the four red-red shapes.
module RedBlackTree =

    /// Node colour used by the balancing scheme.
    type Color =
        | Red
        | Black

    /// A coloured node, or <c>Empty</c> (which is treated as black).
    type Tree<'T> =
        | Empty
        | Node of Color * Tree<'T> * 'T * Tree<'T>

    /// The empty tree.
    let empty<'T> : Tree<'T> = Empty

    /// Whether the value is present.
    let rec contains value tree =
        match tree with
        | Empty -> false
        | Node(_, l, v, r) ->
            if value < v then contains value l
            elif value > v then contains value r
            else true

    let private balance color (left, value, right) =
        match color, left, value, right with
        | Black, Node(Red, Node(Red, a, x, b), y, c), z, d
        | Black, Node(Red, a, x, Node(Red, b, y, c)), z, d
        | Black, a, x, Node(Red, Node(Red, b, y, c), z, d)
        | Black, a, x, Node(Red, b, y, Node(Red, c, z, d)) ->
            Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        | _ -> Node(color, left, value, right)

    /// Inserts a value and restores the red-black invariants. Duplicates are ignored.
    let insert value tree =
        let rec ins t =
            match t with
            | Empty -> Node(Red, Empty, value, Empty)
            | Node(color, l, v, r) ->
                if value < v then balance color (ins l, v, r)
                elif value > v then balance color (l, v, ins r)
                else t

        match ins tree with
        | Node(_, l, v, r) -> Node(Black, l, v, r)
        | Empty -> Empty

    /// Builds a balanced tree from a sequence of values.
    let ofSeq items : Tree<'T> =
        items |> Seq.fold (fun tree x -> insert x tree) empty

    /// In-order (ascending) enumeration.
    let toList tree =
        let rec loop acc t =
            match t with
            | Empty -> acc
            | Node(_, l, v, r) -> loop (v :: loop acc r) l
        loop [] tree

    /// The number of values stored.
    let rec count tree =
        match tree with
        | Empty -> 0
        | Node(_, l, _, r) -> 1 + count l + count r

    /// The number of levels on the longest root-to-leaf path.
    let rec height tree =
        match tree with
        | Empty -> 0
        | Node(_, l, _, r) -> 1 + max (height l) (height r)

    /// Validates the red-black invariants: a black root, no red node with a red
    /// child, an equal black-height on every path, and in-order sortedness.
    let isValid tree =
        let rootBlack =
            match tree with
            | Empty -> true
            | Node(c, _, _, _) -> c = Black

        let redChild n =
            match n with
            | Node(Red, _, _, _) -> true
            | _ -> false

        let rec noRedRed t =
            match t with
            | Empty -> true
            | Node(Red, l, _, r) -> not (redChild l) && not (redChild r) && noRedRed l && noRedRed r
            | Node(Black, l, _, r) -> noRedRed l && noRedRed r

        let rec blackHeight t =
            match t with
            | Empty -> Some 1
            | Node(c, l, _, r) ->
                match blackHeight l, blackHeight r with
                | Some bl, Some br when bl = br -> Some(bl + (if c = Black then 1 else 0))
                | _ -> None

        let sorted =
            let values = toList tree
            values = List.sort values

        rootBlack && noRedRed tree && Option.isSome (blackHeight tree) && sorted

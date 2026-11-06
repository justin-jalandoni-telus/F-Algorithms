namespace Algorithms

/// An immutable binary tree with no ordering invariant, plus the standard
/// traversals. Ordered variants live in <see cref="Bst"/>, <see cref="AvlTree"/>,
/// and <see cref="RedBlackTree"/>.
module BinaryTree =

    /// A binary tree node, or <c>Empty</c>.
    type Tree<'T> =
        | Empty
        | Node of Tree<'T> * 'T * Tree<'T>

    /// The empty tree.
    let empty<'T> : Tree<'T> = Empty

    /// A single-value tree.
    let leaf value = Node(Empty, value, Empty)

    /// Builds a node from its parts.
    let node left value right = Node(left, value, right)

    /// Whether the tree has no nodes.
    let isEmpty tree =
        match tree with
        | Empty -> true
        | _ -> false

    /// The number of nodes in the tree.
    let rec size tree =
        match tree with
        | Empty -> 0
        | Node(l, _, r) -> 1 + size l + size r

    /// The number of levels on the longest root-to-leaf path.
    let rec height tree =
        match tree with
        | Empty -> 0
        | Node(l, _, r) -> 1 + max (height l) (height r)

    /// Applies <paramref name="f"/> to every value, preserving shape.
    let rec map f tree =
        match tree with
        | Empty -> Empty
        | Node(l, v, r) -> Node(map f l, f v, map f r)

    /// In-order fold threading an accumulator left to right.
    let rec fold folder state tree =
        match tree with
        | Empty -> state
        | Node(l, v, r) ->
            let afterLeft = fold folder state l
            fold folder (folder afterLeft v) r

    /// Node, then left subtree, then right subtree.
    let rec preOrder tree =
        match tree with
        | Empty -> []
        | Node(l, v, r) -> v :: (preOrder l @ preOrder r)

    /// Left subtree, then node, then right subtree.
    let rec inOrder tree =
        match tree with
        | Empty -> []
        | Node(l, v, r) -> inOrder l @ (v :: inOrder r)

    /// Left subtree, then right subtree, then node.
    let rec postOrder tree =
        match tree with
        | Empty -> []
        | Node(l, v, r) -> postOrder l @ postOrder r @ [ v ]

    /// Breadth-first (level-order) enumeration.
    let levelOrder tree =
        let rec loop queue acc =
            match queue with
            | [] -> List.rev acc
            | Empty :: rest -> loop rest acc
            | Node(l, v, r) :: rest -> loop (rest @ [ l; r ]) (v :: acc)
        loop [ tree ] []

    /// In-order enumeration (sorted when the tree is a search tree).
    let toList tree = inOrder tree

namespace Algorithms

/// An immutable B-tree of configurable minimum degree <c>t</c>: every node
/// holds between <c>t-1</c> and <c>2t-1</c> keys (the root may hold fewer), and
/// all leaves share the same depth. Supports insertion, membership, and
/// in-order enumeration.
module BTree =

    /// A B-tree node: sorted <see cref="Keys"/> and, for internal nodes,
    /// one more child than keys. Leaves have no children.
    type Node<'T> =
        { Keys: 'T list
          Children: Node<'T> list }

    /// A B-tree with its minimum degree and root node.
    type Tree<'T> =
        { Degree: int
          Root: Node<'T> }

    let private isLeaf (node: Node<'T>) = List.isEmpty node.Children

    let private childIndex key keys =
        keys |> List.filter (fun k -> k < key) |> List.length

    let private insertSorted key keys =
        let rec loop ks =
            match ks with
            | [] -> [ key ]
            | k :: rest ->
                if key < k then key :: ks
                elif key = k then ks
                else k :: loop rest
        loop keys

    let private insertAt index value list =
        List.truncate index list @ [ value ] @ List.skip index list

    let private updateAt index value list =
        list |> List.mapi (fun i x -> if i = index then value else x)

    let private replaceWithTwo index (first, second) list =
        List.truncate index list @ [ first; second ] @ List.skip (index + 1) list

    let private splitChild degree (node: Node<'T>) =
        let median = node.Keys.[degree - 1]
        let leftKeys = List.truncate (degree - 1) node.Keys
        let rightKeys = List.skip degree node.Keys

        let leftChildren, rightChildren =
            if isLeaf node then [], []
            else List.truncate degree node.Children, List.skip degree node.Children

        { Keys = leftKeys; Children = leftChildren },
        median,
        { Keys = rightKeys; Children = rightChildren }

    /// Creates an empty B-tree with the given minimum degree (>= 2).
    let create degree =
        if degree < 2 then
            invalidArg (nameof degree) "minimum degree must be >= 2"
        { Degree = degree; Root = { Keys = []; Children = [] } }

    /// Whether the key is present.
    let contains key (tree: Tree<'T>) =
        let rec go (node: Node<'T>) =
            if List.contains key node.Keys then true
            elif isLeaf node then false
            else go node.Children.[childIndex key node.Keys]
        go tree.Root

    let rec private insertNonFull degree key (node: Node<'T>) =
        if isLeaf node then
            { node with Keys = insertSorted key node.Keys }
        else
            let index = childIndex key node.Keys
            let child = node.Children.[index]

            if List.length child.Keys = 2 * degree - 1 then
                let left, median, right = splitChild degree child
                let keys = insertAt index median node.Keys
                let children = replaceWithTwo index (left, right) node.Children
                let targetIndex = if key > median then index + 1 else index
                let updated = insertNonFull degree key children.[targetIndex]
                { Keys = keys; Children = updateAt targetIndex updated children }
            else
                let updated = insertNonFull degree key child
                { node with Children = updateAt index updated node.Children }

    /// Inserts a key, splitting full nodes top-down. Duplicates are ignored.
    let insert key tree =
        if contains key tree then
            tree
        else
            let degree = tree.Degree
            let root = tree.Root

            if List.length root.Keys = 2 * degree - 1 then
                let left, median, right = splitChild degree root
                let grown = { Keys = [ median ]; Children = [ left; right ] }
                { tree with Root = insertNonFull degree key grown }
            else
                { tree with Root = insertNonFull degree key root }

    /// Builds a B-tree of the given degree from a sequence of keys.
    let ofSeq degree items =
        items |> Seq.fold (fun tree x -> insert x tree) (create degree)

    /// In-order (ascending) enumeration of all keys.
    let toList (tree: Tree<'T>) =
        let rec collect (node: Node<'T>) =
            if isLeaf node then
                node.Keys
            else
                let rec interleave children keys =
                    match children, keys with
                    | c :: cs, k :: ks -> collect c @ (k :: interleave cs ks)
                    | [ c ], [] -> collect c
                    | _ -> []
                interleave node.Children node.Keys
        collect tree.Root

    /// The number of keys stored.
    let count (tree: Tree<'T>) =
        let rec go (node: Node<'T>) =
            List.length node.Keys + (node.Children |> List.sumBy go)
        go tree.Root

    /// The number of node levels (0 for an empty tree).
    let height (tree: Tree<'T>) =
        let rec levels (node: Node<'T>) =
            if isLeaf node then (if List.isEmpty node.Keys then 0 else 1)
            else 1 + levels (List.head node.Children)
        levels tree.Root

    /// Validates the B-tree invariants: uniform leaf depth, key counts within
    /// <c>[t-1, 2t-1]</c> for non-root nodes, matching child counts, and order.
    let isValid (tree: Tree<'T>) =
        let degree = tree.Degree

        let rec leafDepths (node: Node<'T>) depth =
            if isLeaf node then [ depth ]
            else node.Children |> List.collect (fun c -> leafDepths c (depth + 1))

        let depthsUniform =
            match leafDepths tree.Root 0 |> List.distinct with
            | []
            | [ _ ] -> true
            | _ -> false

        let rec structureOk isRoot (node: Node<'T>) =
            let n = List.length node.Keys
            let upperOk = n <= 2 * degree - 1
            let lowerOk = isRoot || n >= degree - 1

            let childrenOk =
                isLeaf node
                || (List.length node.Children = n + 1
                    && node.Children |> List.forall (structureOk false))

            upperOk && lowerOk && childrenOk

        let ordered =
            let values = toList tree
            values = List.sort values

        depthsUniform && structureOk true tree.Root && ordered

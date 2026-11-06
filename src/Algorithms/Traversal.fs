namespace Algorithms

/// Graph traversals returning vertices in visit order.
[<RequireQualifiedAccess>]
module Traversal =

    let private sortedNeighbours vertex graph =
        Graph.neighbours vertex graph |> List.map fst |> List.sort

    /// Depth-first search from <paramref name="start"/>, iterative to avoid
    /// stack overflow on deep graphs.
    let depthFirst start (graph: Graph) : Vertex list =
        let rec loop stack visited order =
            match stack with
            | [] -> List.rev order
            | current :: rest when Set.contains current visited -> loop rest visited order
            | current :: rest ->
                let next = sortedNeighbours current graph
                loop (next @ rest) (Set.add current visited) (current :: order)

        if Graph.containsVertex start graph then loop [ start ] Set.empty []
        else []

    /// Breadth-first search from <paramref name="start"/> using an immutable queue.
    let breadthFirst start (graph: Graph) : Vertex list =
        let rec loop queue visited order =
            match queue with
            | [] -> List.rev order
            | current :: rest when Set.contains current visited -> loop rest visited order
            | current :: rest ->
                let unseen =
                    sortedNeighbours current graph
                    |> List.filter (fun v -> not (Set.contains v visited))
                loop (rest @ unseen) (Set.add current visited) (current :: order)

        if Graph.containsVertex start graph then loop [ start ] Set.empty []
        else []

    /// Whether <paramref name="target"/> is reachable from <paramref name="start"/>.
    let isReachable start target (graph: Graph) : bool =
        depthFirst start graph |> List.contains target

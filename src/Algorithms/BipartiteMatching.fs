namespace Algorithms

/// Maximum bipartite matching via Kuhn's augmenting-path algorithm. The caller
/// supplies the left-side vertices; edges are followed from the graph adjacency
/// (model the graph with edges from left to right, or as undirected).
[<RequireQualifiedAccess>]
module BipartiteMatching =

    /// The maximum matching as a list of (left, right) vertex pairs.
    let maximumMatching (leftVertices: Vertex list) (graph: Graph) : (Vertex * Vertex) list =
        let neighbours vertex = Graph.neighbours vertex graph |> List.map fst

        // Try to find an augmenting path from a left vertex, threading the
        // visited-right set and the right→left matching. Returns whether a path
        // was found alongside the updated state.
        let rec augment left visited matching =
            neighbours left
            |> List.fold
                (fun (found, visited, matching) right ->
                    if found || Set.contains right visited then
                        found, visited, matching
                    else
                        let visited = Set.add right visited

                        match Map.tryFind right matching with
                        | None -> true, visited, Map.add right left matching
                        | Some owner ->
                            let reassigned, visited, matching = augment owner visited matching

                            if reassigned then true, visited, Map.add right left matching
                            else false, visited, matching)
                (false, visited, matching)

        let finalMatching =
            leftVertices
            |> List.fold
                (fun matching left ->
                    let _, _, matching = augment left Set.empty matching
                    matching)
                Map.empty

        finalMatching |> Map.toList |> List.map (fun (right, left) -> left, right)

namespace Algorithms

/// Minimum-spanning-tree and -forest algorithms for undirected graphs.
[<RequireQualifiedAccess>]
module Mst =

    /// Immutable disjoint-set (union-find) keyed by vertex.
    type private DisjointSet = Map<Vertex, Vertex>

    let private makeSet vertices : DisjointSet =
        vertices |> List.map (fun v -> v, v) |> Map.ofList

    let rec private find vertex (sets: DisjointSet) =
        match Map.tryFind vertex sets with
        | Some parent when parent = vertex -> vertex
        | Some parent -> find parent sets
        | None -> vertex

    let private union a b (sets: DisjointSet) : DisjointSet =
        let rootA = find a sets
        let rootB = find b sets
        if rootA = rootB then sets else Map.add rootA rootB sets

    /// Kruskal's algorithm. Returns the chosen edges of a minimum spanning
    /// tree (or forest, if the graph is disconnected). Directed input is
    /// treated as its underlying undirected edge set.
    let kruskal (graph: Graph) : Edge list =
        let sortedEdges = Graph.edges graph |> List.sortBy (fun e -> e.Weight)

        let folder (chosen, sets) (edge: Edge) =
            if find edge.Source sets = find edge.Target sets then chosen, sets
            else edge :: chosen, union edge.Source edge.Target sets

        let initial = [], makeSet (Graph.vertices graph)
        let chosen, _ = List.fold folder initial sortedEdges
        List.rev chosen

    /// Prim's algorithm (lazy variant). Grows a minimum spanning tree from each
    /// unvisited vertex, so the result is a spanning forest for disconnected
    /// graphs — matching <see cref="kruskal"/>. Directed input is treated as its
    /// underlying undirected edge set via the stored adjacency.
    let prim (graph: Graph) : Edge list =
        let candidatesFrom vertex =
            Graph.neighbours vertex graph
            |> List.map (fun (target, weight) -> Edge.create vertex target weight)

        let rec grow visited candidates acc =
            match candidates with
            | [] -> visited, acc
            | _ ->
                let edge = candidates |> List.minBy (fun e -> e.Weight)
                let rest = candidates |> List.filter (fun e -> e <> edge)

                if Set.contains edge.Target visited then
                    grow visited rest acc
                else
                    grow (Set.add edge.Target visited) (candidatesFrom edge.Target @ rest) (edge :: acc)

        let startAt (visited, acc) start =
            if Set.contains start visited then visited, acc
            else grow (Set.add start visited) (candidatesFrom start) acc

        Graph.vertices graph |> List.fold startAt (Set.empty, []) |> snd |> List.rev

    /// The total weight of a set of edges.
    let totalWeight (edges: Edge list) : float =
        edges |> List.sumBy (fun e -> e.Weight)

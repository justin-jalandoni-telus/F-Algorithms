namespace Algorithms

/// Result of a single-source shortest-path search.
type ShortestPathResult =
    { Distances: Map<Vertex, float>
      Predecessors: Map<Vertex, Vertex> }

/// Outcome of Bellman-Ford, which can detect negative cycles.
type BellmanFordResult =
    | Paths of ShortestPathResult
    | NegativeCycle

[<RequireQualifiedAccess>]
module ShortestPath =

    let private infinity = System.Double.PositiveInfinity

    let private distanceTo vertex distances =
        Map.tryFind vertex distances |> Option.defaultValue infinity

    /// Reconstructs the path to <paramref name="target"/> from a result's
    /// predecessor map. Returns an empty list if the target is unreachable.
    let reconstructPath target (result: ShortestPathResult) : Vertex list =
        let rec walk current acc =
            match Map.tryFind current result.Predecessors with
            | Some predecessor -> walk predecessor (current :: acc)
            | None -> current :: acc

        if Map.containsKey target result.Distances
           && result.Distances.[target] <> infinity then
            walk target []
        else
            []

    /// Dijkstra's algorithm. Requires non-negative edge weights.
    let dijkstra source (graph: Graph) : ShortestPathResult =
        let rec loop (distances, predecessors) visited frontier =
            if Set.isEmpty frontier then
                { Distances = distances; Predecessors = predecessors }
            else
                let current = frontier |> Set.toSeq |> Seq.minBy (fun v -> distanceTo v distances)
                let currentDistance = distanceTo current distances

                let relax (dist, pred, front) (neighbour, weight) =
                    let candidate = currentDistance + weight
                    if candidate < distanceTo neighbour dist then
                        Map.add neighbour candidate dist,
                        Map.add neighbour current pred,
                        Set.add neighbour front
                    else
                        dist, pred, front

                let openNeighbours =
                    Graph.neighbours current graph
                    |> List.filter (fun (v, _) -> not (Set.contains v visited))

                let nextDistances, nextPredecessors, nextFrontier =
                    List.fold relax (distances, predecessors, frontier) openNeighbours

                loop (nextDistances, nextPredecessors) (Set.add current visited) (Set.remove current nextFrontier)

        loop (Map.ofList [ source, 0.0 ], Map.empty) Set.empty (Set.singleton source)

    let private directedEdges (graph: Graph) =
        let edges = Graph.edges graph
        match graph.Direction with
        | Directed -> edges
        | Undirected ->
            edges |> List.collect (fun e -> [ e; Edge.create e.Target e.Source e.Weight ])

    /// Bellman-Ford. Handles negative weights and reports negative cycles.
    let bellmanFord source (graph: Graph) : BellmanFordResult =
        let vertices = Graph.vertices graph
        let edges = directedEdges graph

        let relaxOnce (distances, predecessors) (edge: Edge) =
            let sourceDistance = distanceTo edge.Source distances
            let candidate = sourceDistance + edge.Weight
            if sourceDistance <> infinity && candidate < distanceTo edge.Target distances then
                Map.add edge.Target candidate distances, Map.add edge.Target edge.Source predecessors
            else
                distances, predecessors

        let relaxAll state = List.fold relaxOnce state edges
        let initial = Map.ofList [ source, 0.0 ], Map.empty

        let distances, predecessors =
            [ 1 .. List.length vertices - 1 ] |> List.fold (fun state _ -> relaxAll state) initial

        let hasNegativeCycle =
            edges
            |> List.exists (fun edge ->
                let sourceDistance = distanceTo edge.Source distances
                sourceDistance <> infinity
                && sourceDistance + edge.Weight < distanceTo edge.Target distances)

        if hasNegativeCycle then NegativeCycle
        else Paths { Distances = distances; Predecessors = predecessors }

    /// Floyd-Warshall all-pairs shortest paths. Returns the matrix whose
    /// indexing is reused, plus the computed distance grid.
    let floydWarshall (graph: Graph) : AdjacencyMatrix * float[][] =
        let matrix = AdjacencyMatrix.ofGraph graph
        let size = matrix.Vertices.Length

        let distances =
            Array.init size (fun row ->
                Array.init size (fun col ->
                    if row = col then 0.0
                    else matrix.Weights.[row].[col] |> Option.defaultValue infinity))

        for k in 0 .. size - 1 do
            for i in 0 .. size - 1 do
                for j in 0 .. size - 1 do
                    let throughK = distances.[i].[k] + distances.[k].[j]
                    if throughK < distances.[i].[j] then
                        distances.[i].[j] <- throughK

        matrix, distances

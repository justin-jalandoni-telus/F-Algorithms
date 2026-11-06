namespace Algorithms

/// Maximum flow on a directed, capacitated graph via the Edmonds–Karp algorithm
/// (BFS-augmenting Ford–Fulkerson). Edge weights are capacities. Assumes no
/// antiparallel edges (not both u→v and v→u); split a vertex if you need them.
[<RequireQualifiedAccess>]
module MaxFlow =

    /// Residual capacities keyed by (from) then (to).
    type private Residual = Map<Vertex, Map<Vertex, float>>

    let private capacity source target (residual: Residual) =
        residual
        |> Map.tryFind source
        |> Option.bind (Map.tryFind target)
        |> Option.defaultValue 0.0

    let private addCapacity (source, target) amount (residual: Residual) : Residual =
        let inner = residual |> Map.tryFind source |> Option.defaultValue Map.empty
        let updated = (inner |> Map.tryFind target |> Option.defaultValue 0.0) + amount
        Map.add source (Map.add target updated inner) residual

    let private buildResidual (graph: Graph) : Residual =
        Graph.edges graph
        |> List.fold
            (fun residual edge ->
                residual
                |> addCapacity (edge.Source, edge.Target) edge.Weight
                |> addCapacity (edge.Target, edge.Source) 0.0)
            Map.empty

    let private residualNeighbours vertex (residual: Residual) =
        residual
        |> Map.tryFind vertex
        |> Option.defaultValue Map.empty
        |> Map.toList
        |> List.choose (fun (target, cap) -> if cap > 0.0 then Some target else None)

    /// BFS over residual edges; returns the child→parent map once the sink is
    /// reached, or <c>None</c> if no augmenting path remains.
    let private bfsAugment source sink (residual: Residual) =
        let rec loop queue parents =
            match queue with
            | [] -> None
            | current :: _ when current = sink -> Some parents
            | current :: rest ->
                let unseen =
                    residualNeighbours current residual
                    |> List.filter (fun v -> v <> source && not (Map.containsKey v parents))

                let parents' = unseen |> List.fold (fun m v -> Map.add v current m) parents
                loop (rest @ unseen) parents'

        loop [ source ] Map.empty

    let private tracePath sink parents =
        let rec build node acc =
            match Map.tryFind node parents with
            | Some parent -> build parent (node :: acc)
            | None -> node :: acc

        build sink []

    let private bottleneck path (residual: Residual) =
        path |> List.pairwise |> List.map (fun (u, v) -> capacity u v residual) |> List.min

    let private augment path amount (residual: Residual) : Residual =
        path
        |> List.pairwise
        |> List.fold (fun r (u, v) -> r |> addCapacity (u, v) (-amount) |> addCapacity (v, u) amount) residual

    let private run source sink (graph: Graph) =
        let rec loop residual value =
            match bfsAugment source sink residual with
            | None -> residual, value
            | Some parents ->
                let path = tracePath sink parents
                let f = bottleneck path residual
                loop (augment path f residual) (value + f)

        loop (buildResidual graph) 0.0

    /// The maximum flow value from <paramref name="source"/> to
    /// <paramref name="sink"/>.
    let maxFlow source sink (graph: Graph) : float = run source sink graph |> snd

    /// The edges crossing a minimum cut. By the max-flow/min-cut theorem their
    /// total capacity equals <see cref="maxFlow"/>.
    let minCut source sink (graph: Graph) : Edge list =
        let residual, _ = run source sink graph

        let rec reach frontier visited =
            match frontier with
            | [] -> visited
            | current :: rest when Set.contains current visited -> reach rest visited
            | current :: rest -> reach (residualNeighbours current residual @ rest) (Set.add current visited)

        let reachable = reach [ source ] Set.empty

        Graph.edges graph
        |> List.filter (fun e -> Set.contains e.Source reachable && not (Set.contains e.Target reachable))

namespace Algorithms

/// An immutable graph stored as an adjacency list. <see cref="Adjacency"/>
/// maps each vertex to its outgoing weighted neighbours.
type Graph =
    { Direction: Direction
      Adjacency: Map<Vertex, Map<Vertex, float>> }

/// Construction and query operations over the adjacency-list <see cref="Graph"/>.
[<RequireQualifiedAccess>]
module Graph =

    /// An empty graph with the given edge direction.
    let empty direction : Graph =
        { Direction = direction; Adjacency = Map.empty }

    let private ensureVertex vertex adjacency =
        if Map.containsKey vertex adjacency then adjacency
        else Map.add vertex Map.empty adjacency

    /// Adds an isolated vertex, leaving existing edges untouched.
    let addVertex vertex (graph: Graph) : Graph =
        { graph with Adjacency = ensureVertex vertex graph.Adjacency }

    let private connect (source, target, weight) adjacency =
        let neighbours =
            adjacency
            |> Map.tryFind source
            |> Option.defaultValue Map.empty
            |> Map.add target weight
        Map.add source neighbours adjacency

    /// Adds an edge, creating endpoints as needed. Undirected graphs gain
    /// the mirrored edge automatically.
    let addEdge (edge: Edge) (graph: Graph) : Graph =
        let withEndpoints =
            graph.Adjacency
            |> ensureVertex edge.Source
            |> ensureVertex edge.Target

        let forward = connect (edge.Source, edge.Target, edge.Weight) withEndpoints

        let adjacency =
            match graph.Direction with
            | Directed -> forward
            | Undirected -> connect (edge.Target, edge.Source, edge.Weight) forward

        { graph with Adjacency = adjacency }

    /// Builds a graph from a sequence of edges.
    let ofEdges direction edges : Graph =
        edges |> Seq.fold (fun graph edge -> addEdge edge graph) (empty direction)

    /// All vertices in ascending label order.
    let vertices (graph: Graph) : Vertex list =
        graph.Adjacency |> Map.toList |> List.map fst

    /// The number of vertices in the graph.
    let vertexCount (graph: Graph) : int = Map.count graph.Adjacency

    /// The outgoing neighbours of a vertex paired with their edge weights.
    let neighbours vertex (graph: Graph) : (Vertex * float) list =
        graph.Adjacency
        |> Map.tryFind vertex
        |> Option.map Map.toList
        |> Option.defaultValue []

    /// Whether the vertex is present in the graph.
    let containsVertex vertex (graph: Graph) : bool =
        Map.containsKey vertex graph.Adjacency

    /// The weight of the edge from <paramref name="source"/> to
    /// <paramref name="target"/>, if one exists.
    let tryWeight source target (graph: Graph) : float option =
        graph.Adjacency
        |> Map.tryFind source
        |> Option.bind (Map.tryFind target)

    /// All edges in the graph. For undirected graphs each pair appears once,
    /// using the lower vertex id as the source.
    let edges (graph: Graph) : Edge list =
        let directed =
            [ for KeyValue (source, neighbours) in graph.Adjacency do
                  for KeyValue (target, weight) in neighbours do
                      yield Edge.create source target weight ]

        match graph.Direction with
        | Directed -> directed
        | Undirected -> directed |> List.filter (fun e -> e.Source <= e.Target)

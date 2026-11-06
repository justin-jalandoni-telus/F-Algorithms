namespace Algorithms

/// An explicit adjacency-list representation: each vertex mapped to its
/// weighted neighbours. This is the list counterpart to
/// <see cref="AdjacencyMatrix"/>. (The core <see cref="Graph"/> type is itself
/// backed by an adjacency list; this exposes that view as a standalone value
/// that round-trips to and from a graph.)
type AdjacencyList =
    { Vertices: Vertex list
      Neighbours: Map<Vertex, (Vertex * float) list> }

[<RequireQualifiedAccess>]
module AdjacencyList =

    /// Projects a graph into an explicit adjacency list.
    let ofGraph (graph: Graph) : AdjacencyList =
        let vertices = Graph.vertices graph
        { Vertices = vertices
          Neighbours = vertices |> List.map (fun v -> v, Graph.neighbours v graph) |> Map.ofList }

    /// Rebuilds a graph from an adjacency list with the given direction.
    let toGraph direction (list: AdjacencyList) : Graph =
        let withVertices =
            list.Vertices |> List.fold (fun g v -> Graph.addVertex v g) (Graph.empty direction)

        list.Neighbours
        |> Map.toList
        |> List.collect (fun (source, ns) -> ns |> List.map (fun (target, weight) -> Edge.create source target weight))
        |> List.fold (fun g e -> Graph.addEdge e g) withVertices

    /// The weighted neighbours of a vertex.
    let neighbours vertex (list: AdjacencyList) : (Vertex * float) list =
        Map.tryFind vertex list.Neighbours |> Option.defaultValue []

    /// The weight of the edge from <paramref name="source"/> to
    /// <paramref name="target"/>, if present.
    let tryWeight source target (list: AdjacencyList) : float option =
        Map.tryFind source list.Neighbours
        |> Option.bind (List.tryFind (fun (t, _) -> t = target))
        |> Option.map snd

namespace Algorithms

/// A dense adjacency-matrix view of a graph. <see cref="Index"/> maps a vertex
/// to its row/column; <c>Weights.[i].[j]</c> holds the edge weight or <c>None</c>.
type AdjacencyMatrix =
    { Vertices: Vertex[]
      Index: Map<Vertex, int>
      Weights: float option[][] }

[<RequireQualifiedAccess>]
module AdjacencyMatrix =

    /// Projects a graph into a dense adjacency matrix.
    let ofGraph (graph: Graph) : AdjacencyMatrix =
        let vertices = Graph.vertices graph |> List.sort |> List.toArray
        let index = vertices |> Array.mapi (fun i v -> v, i) |> Map.ofArray
        let size = vertices.Length

        let weights =
            Array.init size (fun row ->
                Array.init size (fun col ->
                    Graph.tryWeight vertices.[row] vertices.[col] graph))

        { Vertices = vertices; Index = index; Weights = weights }

    /// Rebuilds a graph from a matrix with the given direction.
    let toGraph direction (matrix: AdjacencyMatrix) : Graph =
        let edges =
            [ for row in 0 .. matrix.Vertices.Length - 1 do
                  for col in 0 .. matrix.Vertices.Length - 1 do
                      match matrix.Weights.[row].[col] with
                      | Some weight ->
                          yield Edge.create matrix.Vertices.[row] matrix.Vertices.[col] weight
                      | None -> () ]
        Graph.ofEdges direction edges

    /// The weight stored for a vertex pair, if both vertices exist and are linked.
    let tryWeight source target (matrix: AdjacencyMatrix) : float option =
        match Map.tryFind source matrix.Index, Map.tryFind target matrix.Index with
        | Some row, Some col -> matrix.Weights.[row].[col]
        | _ -> None

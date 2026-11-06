namespace Algorithms

/// A graph vertex identified by an integer label.
type Vertex = int

/// A weighted directed connection from <see cref="Source"/> to <see cref="Target"/>.
type Edge =
    { Source: Vertex
      Target: Vertex
      Weight: float }

/// Whether edges are one-way or symmetric.
type Direction =
    | Directed
    | Undirected

[<RequireQualifiedAccess>]
module Edge =

    /// Creates a weighted edge.
    let create source target weight =
        { Source = source; Target = target; Weight = weight }

    /// Creates an edge with the default weight of 1.0.
    let unweighted source target = create source target 1.0

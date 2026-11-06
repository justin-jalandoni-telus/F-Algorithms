module Algorithms.Tests.AdjacencyMatrixTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``matrix round-trips to graph`` () =
    let graph = Graph.ofEdges Undirected [ Edge.create 1 2 2.0; Edge.create 2 3 3.0 ]
    let restored = graph |> AdjacencyMatrix.ofGraph |> AdjacencyMatrix.toGraph Undirected
    Graph.tryWeight 1 2 restored |> should equal (Some 2.0)
    Graph.tryWeight 3 2 restored |> should equal (Some 3.0)

[<Fact>]
let ``matrix stores directed weights by index`` () =
    let matrix = Graph.ofEdges Directed [ Edge.create 1 2 2.0 ] |> AdjacencyMatrix.ofGraph
    AdjacencyMatrix.tryWeight 1 2 matrix |> should equal (Some 2.0)
    AdjacencyMatrix.tryWeight 2 1 matrix |> should equal None

[<Fact>]
let ``absent vertices yield no weight`` () =
    let matrix = Graph.ofEdges Directed [ Edge.create 1 2 2.0 ] |> AdjacencyMatrix.ofGraph
    AdjacencyMatrix.tryWeight 1 99 matrix |> should equal None

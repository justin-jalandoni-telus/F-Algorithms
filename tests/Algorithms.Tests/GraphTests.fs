module Algorithms.Tests.GraphTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``undirected edge is mirrored`` () =
    let graph = Graph.empty Undirected |> Graph.addEdge (Edge.create 1 2 5.0)
    Graph.tryWeight 1 2 graph |> should equal (Some 5.0)
    Graph.tryWeight 2 1 graph |> should equal (Some 5.0)

[<Fact>]
let ``directed edge is one-way`` () =
    let graph = Graph.empty Directed |> Graph.addEdge (Edge.create 1 2 5.0)
    Graph.tryWeight 1 2 graph |> should equal (Some 5.0)
    Graph.tryWeight 2 1 graph |> should equal None

[<Fact>]
let ``ofEdges records every vertex`` () =
    let graph = Graph.ofEdges Directed [ Edge.unweighted 1 2; Edge.unweighted 2 3 ]
    Graph.vertexCount graph |> should equal 3

[<Fact>]
let ``undirected edges are deduplicated`` () =
    let graph = Graph.ofEdges Undirected [ Edge.create 1 2 1.0; Edge.create 2 3 1.0 ]
    Graph.edges graph |> List.length |> should equal 2

[<Fact>]
let ``neighbours lists outgoing edges in order`` () =
    let graph = Graph.ofEdges Directed [ Edge.create 1 2 4.0; Edge.create 1 3 7.0 ]
    Graph.neighbours 1 graph |> should equal [ (2, 4.0); (3, 7.0) ]

[<Fact>]
let ``addVertex introduces an isolated vertex`` () =
    let graph = Graph.empty Directed |> Graph.addVertex 42
    Graph.containsVertex 42 graph |> should equal true
    Graph.neighbours 42 graph |> List.isEmpty |> should equal true

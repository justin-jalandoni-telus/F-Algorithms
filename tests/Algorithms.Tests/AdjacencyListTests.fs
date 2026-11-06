module Algorithms.Tests.AdjacencyListTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``round-trips to a graph`` () =
    let graph = Graph.ofEdges Undirected [ Edge.create 1 2 2.0; Edge.create 2 3 3.0 ]
    let restored = graph |> AdjacencyList.ofGraph |> AdjacencyList.toGraph Undirected
    Graph.tryWeight 1 2 restored |> should equal (Some 2.0)
    Graph.tryWeight 3 2 restored |> should equal (Some 3.0)

[<Fact>]
let ``lists weighted neighbours in order`` () =
    let list =
        Graph.ofEdges Directed [ Edge.create 1 2 4.0; Edge.create 1 3 7.0 ]
        |> AdjacencyList.ofGraph

    AdjacencyList.neighbours 1 list |> should equal [ (2, 4.0); (3, 7.0) ]

[<Fact>]
let ``directed weight lookup is one-way`` () =
    let list = Graph.ofEdges Directed [ Edge.create 1 2 2.0 ] |> AdjacencyList.ofGraph
    AdjacencyList.tryWeight 1 2 list |> should equal (Some 2.0)
    AdjacencyList.tryWeight 2 1 list |> should equal None

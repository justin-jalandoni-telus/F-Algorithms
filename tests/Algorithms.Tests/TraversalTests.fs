module Algorithms.Tests.TraversalTests

open Xunit
open FsUnit.Xunit
open Algorithms

let private sample () =
    Graph.ofEdges
        Undirected
        [ Edge.unweighted 1 2
          Edge.unweighted 1 3
          Edge.unweighted 2 4
          Edge.unweighted 3 4 ]

[<Fact>]
let ``bfs visits in level order`` () =
    sample () |> Traversal.breadthFirst 1 |> should equal [ 1; 2; 3; 4 ]

[<Fact>]
let ``dfs explores a branch before backtracking`` () =
    sample () |> Traversal.depthFirst 1 |> should equal [ 1; 2; 4; 3 ]

[<Fact>]
let ``reachable vertex is found`` () =
    sample () |> Traversal.isReachable 1 4 |> should equal true

[<Fact>]
let ``unreachable vertex is reported`` () =
    let graph = sample () |> Graph.addVertex 99
    Traversal.isReachable 1 99 graph |> should equal false

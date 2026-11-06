module Algorithms.Tests.ShortestPathTests

open Xunit
open FsUnit.Xunit
open Algorithms

let private weighted () =
    Graph.ofEdges Directed [ Edge.create 1 2 1.0; Edge.create 2 3 2.0; Edge.create 1 3 10.0 ]

[<Fact>]
let ``dijkstra finds the cheapest route`` () =
    let result = ShortestPath.dijkstra 1 (weighted ())
    result.Distances.[3] |> should equal 3.0
    ShortestPath.reconstructPath 3 result |> should equal [ 1; 2; 3 ]

[<Fact>]
let ``bellman-ford matches dijkstra on non-negative graphs`` () =
    match ShortestPath.bellmanFord 1 (weighted ()) with
    | Paths result -> result.Distances.[3] |> should equal 3.0
    | NegativeCycle -> failwith "unexpected negative cycle"

[<Fact>]
let ``bellman-ford detects a negative cycle`` () =
    let graph =
        Graph.ofEdges Directed [ Edge.create 1 2 1.0; Edge.create 2 3 -1.0; Edge.create 3 1 -1.0 ]

    ShortestPath.bellmanFord 1 graph |> should equal NegativeCycle

[<Fact>]
let ``floyd-warshall computes all-pairs distances`` () =
    let _, distances = ShortestPath.floydWarshall (weighted ())
    distances.[0].[2] |> should equal 3.0

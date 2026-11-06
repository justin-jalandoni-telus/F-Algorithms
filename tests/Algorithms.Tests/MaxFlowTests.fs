module Algorithms.Tests.MaxFlowTests

open Xunit
open FsUnit.Xunit
open Algorithms

// Classic 4-node network: s=1, a=2, b=3, t=4. Max flow is 5.
let private network () =
    Graph.ofEdges
        Directed
        [ Edge.create 1 2 3.0 // s -> a
          Edge.create 1 3 2.0 // s -> b
          Edge.create 2 3 1.0 // a -> b
          Edge.create 2 4 2.0 // a -> t
          Edge.create 3 4 3.0 ] // b -> t

[<Fact>]
let ``computes the maximum flow value`` () =
    MaxFlow.maxFlow 1 4 (network ()) |> should equal 5.0

[<Fact>]
let ``min cut capacity equals max flow`` () =
    let graph = network ()
    MaxFlow.minCut 1 4 graph |> Mst.totalWeight |> should equal (MaxFlow.maxFlow 1 4 graph)

[<Fact>]
let ``flow is limited by the bottleneck edge`` () =
    let graph =
        Graph.ofEdges Directed [ Edge.create 1 2 5.0; Edge.create 2 3 1.0; Edge.create 3 4 5.0 ]

    MaxFlow.maxFlow 1 4 graph |> should equal 1.0

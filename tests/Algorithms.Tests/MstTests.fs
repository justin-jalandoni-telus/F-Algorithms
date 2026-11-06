module Algorithms.Tests.MstTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``kruskal selects the minimum spanning tree`` () =
    let graph =
        Graph.ofEdges Undirected [ Edge.create 1 2 1.0; Edge.create 2 3 2.0; Edge.create 1 3 3.0 ]

    let tree = Mst.kruskal graph
    tree |> List.length |> should equal 2
    Mst.totalWeight tree |> should equal 3.0

[<Fact>]
let ``kruskal skips cycle-forming edges`` () =
    let graph =
        Graph.ofEdges Undirected [ Edge.create 1 2 1.0; Edge.create 2 3 1.0; Edge.create 1 3 5.0 ]

    Mst.kruskal graph |> List.exists (fun e -> e.Weight = 5.0) |> should equal false

[<Fact>]
let ``prim selects the minimum spanning tree`` () =
    let graph =
        Graph.ofEdges Undirected [ Edge.create 1 2 1.0; Edge.create 2 3 2.0; Edge.create 1 3 3.0 ]

    let tree = Mst.prim graph
    tree |> List.length |> should equal 2
    Mst.totalWeight tree |> should equal 3.0

[<Fact>]
let ``prim and kruskal agree on total weight`` () =
    let graph =
        Graph.ofEdges
            Undirected
            [ Edge.create 1 2 4.0
              Edge.create 1 3 1.0
              Edge.create 2 3 2.0
              Edge.create 2 4 5.0
              Edge.create 3 4 8.0
              Edge.create 4 5 3.0
              Edge.create 3 5 7.0 ]

    Mst.totalWeight (Mst.prim graph) |> should equal (Mst.totalWeight (Mst.kruskal graph))

[<Fact>]
let ``prim skips cycle-forming edges`` () =
    let graph =
        Graph.ofEdges Undirected [ Edge.create 1 2 1.0; Edge.create 2 3 1.0; Edge.create 1 3 5.0 ]

    Mst.prim graph |> List.exists (fun e -> e.Weight = 5.0) |> should equal false

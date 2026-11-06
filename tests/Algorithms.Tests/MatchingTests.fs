module Algorithms.Tests.MatchingTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``bipartite matching is maximum`` () =
    // left {1,2,3} connected to right {4,5,6}
    let graph =
        Graph.ofEdges
            Directed
            [ Edge.unweighted 1 4
              Edge.unweighted 2 4
              Edge.unweighted 2 5
              Edge.unweighted 3 5
              Edge.unweighted 3 6 ]

    BipartiteMatching.maximumMatching [ 1; 2; 3 ] graph |> List.length |> should equal 3

[<Fact>]
let ``bipartite matching resolves contention`` () =
    // two left vertices competing for a single right vertex
    let graph = Graph.ofEdges Directed [ Edge.unweighted 1 3; Edge.unweighted 2 3 ]
    BipartiteMatching.maximumMatching [ 1; 2 ] graph |> List.length |> should equal 1

[<Fact>]
let ``general matching on a path`` () =
    let graph =
        Graph.ofEdges Undirected [ Edge.unweighted 1 2; Edge.unweighted 2 3; Edge.unweighted 3 4 ]

    GeneralMatching.maximumMatching graph |> List.length |> should equal 2

[<Fact>]
let ``general matching on an odd cycle needs blossoms`` () =
    let graph =
        Graph.ofEdges Undirected [ Edge.unweighted 1 2; Edge.unweighted 2 3; Edge.unweighted 1 3 ]

    GeneralMatching.maximumMatching graph |> List.length |> should equal 1

[<Fact>]
let ``general matching on a five-cycle`` () =
    let graph =
        Graph.ofEdges
            Undirected
            [ Edge.unweighted 1 2
              Edge.unweighted 2 3
              Edge.unweighted 3 4
              Edge.unweighted 4 5
              Edge.unweighted 5 1 ]

    GeneralMatching.maximumMatching graph |> List.length |> should equal 2

[<Fact>]
let ``general matching agrees with bipartite on a bipartite graph`` () =
    let edges =
        [ Edge.unweighted 1 4
          Edge.unweighted 2 4
          Edge.unweighted 2 5
          Edge.unweighted 3 5
          Edge.unweighted 3 6 ]

    let bipartiteSize =
        BipartiteMatching.maximumMatching [ 1; 2; 3 ] (Graph.ofEdges Directed edges)
        |> List.length

    let generalSize =
        GeneralMatching.maximumMatching (Graph.ofEdges Undirected edges) |> List.length

    generalSize |> should equal bipartiteSize

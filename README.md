# Algorithms

A company-wide **F# data-structures and algorithms library**, written in a
functional style with immutable data, small focused functions, and explicit
result types. Every public function is documented and covered by unit tests.

The solution contains two independent libraries so teams can depend on only what
they need:

| Project | Namespace | Focus |
|---|---|---|
| [`src/Algorithms`](src/Algorithms) | `Algorithms` | Graphs, probabilistic structures, functional abstractions, and balanced trees |
| [`src/PurelyFunctional`](src/PurelyFunctional) | `PurelyFunctional` | Persistent structures from Chris Okasaki's *Purely Functional Data Structures* |

Target framework: **.NET 9** (`net9.0`). Both libraries build with
`TreatWarningsAsErrors` enabled.

## Build and test

```bash
dotnet build Algorithms.sln
dotnet test  Algorithms.sln
```

Run a single project's tests:

```bash
dotnet test tests/Algorithms.Tests/Algorithms.Tests.fsproj
dotnet test tests/PurelyFunctional.Tests/PurelyFunctional.Tests.fsproj
```

## `Algorithms` library

### Graphs
Immutable adjacency-list `Graph` with explicit `AdjacencyList` and dense
`AdjacencyMatrix` views, supporting directed and undirected graphs.

- **Representations** — [`Graph`](src/Algorithms/Graph.fs), [`AdjacencyList`](src/Algorithms/AdjacencyList.fs), [`AdjacencyMatrix`](src/Algorithms/AdjacencyMatrix.fs)
- **Traversal** — DFS and BFS, reachability ([`Traversal`](src/Algorithms/Traversal.fs))
- **Shortest paths** — Dijkstra, Bellman–Ford (with negative-cycle detection), Floyd–Warshall ([`ShortestPath`](src/Algorithms/ShortestPath.fs))
- **Minimum spanning tree** — Kruskal and Prim ([`Mst`](src/Algorithms/Mst.fs))
- **Network flow** — Edmonds–Karp max flow and min cut ([`MaxFlow`](src/Algorithms/MaxFlow.fs))
- **Matching** — maximum bipartite matching via Kuhn ([`BipartiteMatching`](src/Algorithms/BipartiteMatching.fs)) and general maximum matching via Edmonds' blossom ([`GeneralMatching`](src/Algorithms/GeneralMatching.fs))

```fsharp
open Algorithms

let graph =
    Graph.ofEdges Directed
        [ Edge.create 1 2 1.0
          Edge.create 2 3 2.0
          Edge.create 1 3 10.0 ]

let result = ShortestPath.dijkstra 1 graph
result.Distances.[3]                    // 3.0
ShortestPath.reconstructPath 3 result   // [1; 2; 3]

MaxFlow.maxFlow 1 3 graph               // max flow from vertex 1 to 3
```

### Probabilistic data structures
Immutable, generic over the element type via a `Hasher<'T>` (64-bit FNV-1a with
Kirsch–Mitzenmacher double hashing, in [`Hash`](src/Algorithms/Hash.fs)).

- [`BloomFilter`](src/Algorithms/BloomFilter.fs) — membership test, no false negatives, tunable false-positive rate
- [`HyperLogLog`](src/Algorithms/HyperLogLog.fs) — approximate distinct count, with `merge`
- [`CountMinSketch`](src/Algorithms/CountMinSketch.fs) — approximate frequencies, never undercounts

```fsharp
let filter = BloomFilter.create Hash.string 1000 0.01 |> BloomFilter.add "hello"
BloomFilter.mightContain "hello" filter   // true
BloomFilter.mightContain "world" filter   // false (definitely absent)
```

### Functional abstractions
[`Monads`](src/Algorithms/Monads.fs) provides `Try`, `Reader`, `State`,
`Either`, `Validation`, `Identity`, and `Exceptional` types (each with a
computation expression where it makes sense), plus Functor/Applicative/Monad
helpers for the built-in `Option` and `Result` types. Every type exposes a
consistent `map`/`retn`/`apply`/`bind` surface, applicative lifting
(`map2`/`map3`), and — for `Option`, `Result`, and `Validation` —
`traverse`/`sequence`. The `Either`, `Validation`, `Identity`, and `Exceptional`
types are ported from Enrico Buonanno's *Functional Programming in C#*
([la-yumba/functional-csharp-code](https://github.com/la-yumba/functional-csharp-code)).

```fsharp
let parsed =
    tryResult {
        let! a = Try.run (fun () -> int "10")
        let! b = Try.run (fun () -> int "32")
        return a + b
    }
parsed |> Try.toOption   // Some 42
```

Unlike `Result`/`Either`, which stop at the first error, the `Validation`
applicative *accumulates* every error — ideal for validating several fields, or
a whole list, at once. `traverse` lifts an element-wise check over a list:

```fsharp
let add a b = a + b

Validation.map2 add (Validation.failWith "name required")
                    (Validation.failWith "age required")
// Invalid [ { Message = "name required" }; { Message = "age required" } ]

let evenOnly x =
    if x % 2 = 0 then Valid x else Validation.failWith $"odd {x}"

[ 1; 2; 3 ] |> Validation.traverse evenOnly
// Invalid [ { Message = "odd 1" }; { Message = "odd 3" } ]

// Monadic traverse/sequence short-circuit instead:
[ Some 1; Some 2; Some 3 ] |> OptionM.sequence   // Some [1; 2; 3]
[ Some 1; None;   Some 3 ] |> OptionM.sequence   // None
```

### Trees
- [`BinaryTree`](src/Algorithms/BinaryTree.fs) — general binary tree with pre/in/post/level-order traversals, `map`, `fold`
- [`Bst`](src/Algorithms/BinarySearchTree.fs) — unbalanced binary search tree
- [`AvlTree`](src/Algorithms/AvlTree.fs) — self-balancing AVL tree (insert/remove, `isBalanced`)
- [`RedBlackTree`](src/Algorithms/RedBlackTree.fs) — Okasaki's functional red-black tree (`isValid`)
- [`BTree`](src/Algorithms/BTree.fs) — B-tree of configurable minimum degree (`isValid`)

```fsharp
let tree = AvlTree.ofSeq [ 5; 3; 8; 1; 4 ]
AvlTree.toList tree        // [1; 3; 4; 5; 8]
AvlTree.isBalanced tree    // true
```

## `PurelyFunctional` library

Persistent structures translated from Okasaki's *Purely Functional Data
Structures*. The chapter each structure comes from is noted in its source file.

- **Lazy streams** — [`Stream`](src/PurelyFunctional/Stream.fs) (Ch. 4)
- **Queues** — [`BatchedQueue`](src/PurelyFunctional/BatchedQueue.fs), [`BankersQueue`](src/PurelyFunctional/BankersQueue.fs), [`PhysicistsQueue`](src/PurelyFunctional/PhysicistsQueue.fs), [`RealTimeQueue`](src/PurelyFunctional/RealTimeQueue.fs) (Ch. 5–7)
- **Deque** — [`BankersDeque`](src/PurelyFunctional/BankersDeque.fs) (Ch. 8)
- **Heaps** — [`LeftistHeap`](src/PurelyFunctional/LeftistHeap.fs), [`BinomialHeap`](src/PurelyFunctional/BinomialHeap.fs), [`PairingHeap`](src/PurelyFunctional/PairingHeap.fs), [`SplayHeap`](src/PurelyFunctional/SplayHeap.fs) (Ch. 3, 5)
- **Random-access lists** — [`BinaryRandomAccessList`](src/PurelyFunctional/BinaryRandomAccessList.fs), [`SkewBinaryRandomAccessList`](src/PurelyFunctional/SkewBinaryRandomAccessList.fs) (Ch. 9)

```fsharp
open PurelyFunctional

BinomialHeap.ofSeq [ 5; 3; 8; 1 ] |> BinomialHeap.toSortedList   // [1; 3; 5; 8]

let q = BankersQueue.ofList [ 1; 2; 3 ]
BankersQueue.head q                      // 1
```

## Design principles

- **Immutability first.** Operations return new values; nothing is mutated in
  place. The few internal mutable loops (e.g. Floyd–Warshall) are isolated and
  referentially transparent.
- **Small, single-purpose functions** with intention-revealing names, following
  the guidance in Robert C. Martin's *Clean Code*.
- **Small signatures.** Every function takes at most three parameters; related
  arguments are grouped into tuples or records (per *Clean Code*'s guidance on
  argument count).
- **Explicit result types** over exceptions where a result can legitimately be
  absent (`BellmanFordResult`, `Try`, `option`).
- **Qualified module access** so call sites read as `Graph.addEdge`,
  `ShortestPath.dijkstra`, `BinomialHeap.insert`.
- **Tested behaviour.** 131 unit tests (xUnit + FsUnit) cover the public API and,
  for the balanced trees and B-trees, assert the structural invariants.

## Project layout

```
Algorithms.sln
Directory.Build.props          # shared: net9.0, XML docs
src/
  Algorithms/                  # graphs, probabilistic DS, monads, trees
  PurelyFunctional/            # Okasaki's persistent structures
tests/
  Algorithms.Tests/
  PurelyFunctional.Tests/
```

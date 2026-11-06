module Algorithms.Tests.RedBlackTreeTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``maintains invariants under ascending inserts`` () =
    RedBlackTree.ofSeq [ 1..200 ] |> RedBlackTree.isValid |> should equal true

[<Fact>]
let ``maintains invariants under shuffled inserts`` () =
    RedBlackTree.ofSeq [ 7; 3; 18; 10; 22; 8; 11; 26; 2; 6; 13 ]
    |> RedBlackTree.isValid
    |> should equal true

[<Fact>]
let ``in-order enumeration is sorted`` () =
    RedBlackTree.ofSeq [ 5; 1; 9; 3; 7 ]
    |> RedBlackTree.toList
    |> should equal [ 1; 3; 5; 7; 9 ]

[<Fact>]
let ``contains finds values`` () =
    let tree = RedBlackTree.ofSeq [ 1..50 ]
    RedBlackTree.contains 25 tree |> should equal true
    RedBlackTree.contains 51 tree |> should equal false

[<Fact>]
let ``height stays logarithmic`` () =
    let tree = RedBlackTree.ofSeq [ 1..1000 ]
    Assert.True(RedBlackTree.height tree <= 20, $"height was {RedBlackTree.height tree}")

module Algorithms.Tests.AvlTreeTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``stays balanced under ascending inserts`` () =
    let tree = AvlTree.ofSeq [ 1..100 ]
    AvlTree.isBalanced tree |> should equal true
    Assert.True(AvlTree.height tree <= 9, $"height was {AvlTree.height tree}")

[<Fact>]
let ``in-order enumeration is sorted`` () =
    AvlTree.ofSeq [ 10; 5; 15; 3; 7; 12; 20; 1 ]
    |> AvlTree.toList
    |> should equal [ 1; 3; 5; 7; 10; 12; 15; 20 ]

[<Fact>]
let ``remove keeps balance and order`` () =
    let tree = AvlTree.ofSeq [ 1..50 ] |> AvlTree.remove 25
    AvlTree.isBalanced tree |> should equal true
    AvlTree.contains 25 tree |> should equal false
    AvlTree.count tree |> should equal 49

[<Fact>]
let ``contains finds values`` () =
    let tree = AvlTree.ofSeq [ 1..20 ]
    AvlTree.contains 15 tree |> should equal true
    AvlTree.contains 21 tree |> should equal false

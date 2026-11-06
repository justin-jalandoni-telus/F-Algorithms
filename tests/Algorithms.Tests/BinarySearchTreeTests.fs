module Algorithms.Tests.BinarySearchTreeTests

open Xunit
open FsUnit.Xunit
open Algorithms

let private build values = Bst.ofSeq values

[<Fact>]
let ``in-order enumeration is sorted`` () =
    build [ 5; 3; 8; 1; 4; 7; 9; 2 ]
    |> Bst.toList
    |> should equal [ 1; 2; 3; 4; 5; 7; 8; 9 ]

[<Fact>]
let ``contains finds inserted values`` () =
    let tree = build [ 5; 3; 8 ]
    Bst.contains 3 tree |> should equal true
    Bst.contains 99 tree |> should equal false

[<Fact>]
let ``remove deletes a value`` () =
    let tree = build [ 5; 3; 8; 4 ] |> Bst.remove 3
    Bst.toList tree |> should equal [ 4; 5; 8 ]
    Bst.contains 3 tree |> should equal false

[<Fact>]
let ``min and max report the extremes`` () =
    let tree = build [ 5; 3; 8; 1 ]
    Bst.tryMin tree |> should equal (Some 1)
    Bst.tryMax tree |> should equal (Some 8)

[<Fact>]
let ``duplicates are ignored`` () =
    build [ 5; 5; 5 ] |> Bst.count |> should equal 1

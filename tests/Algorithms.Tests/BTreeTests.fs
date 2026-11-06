module Algorithms.Tests.BTreeTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``in-order enumeration is sorted`` () =
    BTree.ofSeq 3 [ 10; 20; 5; 6; 12; 30; 7; 17 ]
    |> BTree.toList
    |> should equal [ 5; 6; 7; 10; 12; 17; 20; 30 ]

[<Fact>]
let ``maintains invariants under many inserts`` () =
    let tree = BTree.ofSeq 3 [ 1..200 ]
    BTree.isValid tree |> should equal true
    BTree.count tree |> should equal 200

[<Fact>]
let ``contains finds keys`` () =
    let tree = BTree.ofSeq 4 [ 1..100 ]
    BTree.contains 50 tree |> should equal true
    BTree.contains 101 tree |> should equal false

[<Fact>]
let ``duplicates are ignored`` () =
    BTree.ofSeq 2 [ 5; 5; 5; 3; 3 ] |> BTree.count |> should equal 2

[<Fact>]
let ``stays valid across a range of degrees`` () =
    for degree in [ 2; 3; 5; 10 ] do
        BTree.ofSeq degree [ 1..100 ] |> BTree.isValid |> should equal true

[<Fact>]
let ``invalid degree is rejected`` () =
    (fun () -> BTree.create 1 |> ignore)
    |> should throw typeof<System.ArgumentException>

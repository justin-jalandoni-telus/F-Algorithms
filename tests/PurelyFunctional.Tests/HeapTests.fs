module PurelyFunctional.Tests.HeapTests

open Xunit
open FsUnit.Xunit
open PurelyFunctional

let private sample = [ 5; 3; 8; 1; 9; 2; 7; 4; 6; 0 ]
let private sorted = List.sort sample
let private withDups = [ 3; 1; 3; 2; 1; 2 ]

// A permutation of 0..199 (gcd(73, 200) = 1), used as a larger stress input.
let private largeShuffle = [ for i in 0..199 -> (i * 73 + 31) % 200 ]

[<Fact>]
let ``leftist heap sorts`` () =
    LeftistHeap.ofSeq sample |> LeftistHeap.toSortedList |> should equal sorted

[<Fact>]
let ``leftist heap keeps duplicates`` () =
    LeftistHeap.ofSeq withDups
    |> LeftistHeap.toSortedList
    |> should equal (List.sort withDups)

[<Fact>]
let ``leftist heap reports the minimum`` () =
    LeftistHeap.ofSeq sample |> LeftistHeap.findMin |> should equal 0

[<Fact>]
let ``binomial heap sorts`` () =
    BinomialHeap.ofSeq sample |> BinomialHeap.toSortedList |> should equal sorted

[<Fact>]
let ``binomial heap merges two heaps`` () =
    let a = BinomialHeap.ofSeq [ 1; 3; 5; 7 ]
    let b = BinomialHeap.ofSeq [ 2; 4; 6; 8 ]
    BinomialHeap.merge a b |> BinomialHeap.toSortedList |> should equal [ 1..8 ]

[<Fact>]
let ``pairing heap sorts`` () =
    PairingHeap.ofSeq sample |> PairingHeap.toSortedList |> should equal sorted

[<Fact>]
let ``splay heap sorts`` () =
    SplayHeap.ofSeq sample |> SplayHeap.toSortedList |> should equal sorted

[<Fact>]
let ``all heaps agree on a larger shuffled input`` () =
    let expected = List.sort largeShuffle
    LeftistHeap.ofSeq largeShuffle |> LeftistHeap.toSortedList |> should equal expected
    BinomialHeap.ofSeq largeShuffle |> BinomialHeap.toSortedList |> should equal expected
    PairingHeap.ofSeq largeShuffle |> PairingHeap.toSortedList |> should equal expected
    SplayHeap.ofSeq largeShuffle |> SplayHeap.toSortedList |> should equal expected

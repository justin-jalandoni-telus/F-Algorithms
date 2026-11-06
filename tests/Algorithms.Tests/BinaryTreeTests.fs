module Algorithms.Tests.BinaryTreeTests

open Xunit
open FsUnit.Xunit
open Algorithms

//        1
//       / \
//      2   3
//     /
//    4
let private sample =
    BinaryTree.Node(BinaryTree.Node(BinaryTree.leaf 4, 2, BinaryTree.Empty), 1, BinaryTree.leaf 3)

[<Fact>]
let ``size counts nodes`` () = BinaryTree.size sample |> should equal 4

[<Fact>]
let ``height measures depth`` () = BinaryTree.height sample |> should equal 3

[<Fact>]
let ``in-order traversal`` () =
    BinaryTree.inOrder sample |> should equal [ 4; 2; 1; 3 ]

[<Fact>]
let ``pre-order traversal`` () =
    BinaryTree.preOrder sample |> should equal [ 1; 2; 4; 3 ]

[<Fact>]
let ``post-order traversal`` () =
    BinaryTree.postOrder sample |> should equal [ 4; 2; 3; 1 ]

[<Fact>]
let ``level-order traversal`` () =
    BinaryTree.levelOrder sample |> should equal [ 1; 2; 3; 4 ]

[<Fact>]
let ``map transforms every value`` () =
    sample
    |> BinaryTree.map (fun x -> x * 10)
    |> BinaryTree.inOrder
    |> should equal [ 40; 20; 10; 30 ]

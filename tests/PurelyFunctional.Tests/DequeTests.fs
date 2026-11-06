module PurelyFunctional.Tests.DequeTests

open Xunit
open FsUnit.Xunit
open PurelyFunctional

[<Fact>]
let ``snoc builds front-to-back order`` () =
    BankersDeque.ofList [ 1..20 ] |> BankersDeque.toList |> should equal [ 1..20 ]

[<Fact>]
let ``cons prepends to the front`` () =
    let deque = [ 1; 2; 3 ] |> List.fold (fun d x -> BankersDeque.cons x d) BankersDeque.empty
    BankersDeque.toList deque |> should equal [ 3; 2; 1 ]

[<Fact>]
let ``head and last read both ends`` () =
    let deque = BankersDeque.ofList [ 1..20 ]
    BankersDeque.head deque |> should equal 1
    BankersDeque.last deque |> should equal 20

[<Fact>]
let ``tail and init drop from both ends`` () =
    let deque = BankersDeque.ofList [ 1..20 ]
    deque |> BankersDeque.tail |> BankersDeque.head |> should equal 2
    deque |> BankersDeque.init |> BankersDeque.last |> should equal 19

[<Fact>]
let ``double-ended usage behaves like a deque`` () =
    // cons 3, 2, 1 (so the front reads 1;2;3) then snoc 4, 5, 6  ->  [1;2;3;4;5;6]
    let front = [ 3; 2; 1 ] |> List.fold (fun d x -> BankersDeque.cons x d) BankersDeque.empty
    let full = [ 4; 5; 6 ] |> List.fold (fun d x -> BankersDeque.snoc x d) front
    BankersDeque.toList full |> should equal [ 1; 2; 3; 4; 5; 6 ]

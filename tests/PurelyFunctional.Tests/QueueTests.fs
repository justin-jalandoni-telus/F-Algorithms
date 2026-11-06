module PurelyFunctional.Tests.QueueTests

open Xunit
open FsUnit.Xunit
open PurelyFunctional

// A deterministic permutation of 0..n-1 used to stress the rotations.
let private shuffled n = [ for i in 0 .. n - 1 -> (i * 37 + 11) % n ] |> List.distinct

[<Fact>]
let ``batched queue preserves FIFO order`` () =
    let drain q =
        let rec loop q acc =
            if BatchedQueue.isEmpty q then List.rev acc
            else loop (BatchedQueue.tail q) (BatchedQueue.head q :: acc)

        loop q []

    let input = [ 1..50 ]
    let q = BatchedQueue.ofList input
    q |> BatchedQueue.toList |> should equal input
    drain q |> should equal input

[<Fact>]
let ``bankers queue preserves FIFO order`` () =
    let drain q =
        let rec loop q acc =
            if BankersQueue.isEmpty q then List.rev acc
            else loop (BankersQueue.tail q) (BankersQueue.head q :: acc)

        loop q []

    let input = [ 1..50 ]
    drain (BankersQueue.ofList input) |> should equal input

[<Fact>]
let ``physicists queue preserves FIFO order`` () =
    let drain q =
        let rec loop q acc =
            if PhysicistsQueue.isEmpty q then List.rev acc
            else loop (PhysicistsQueue.tail q) (PhysicistsQueue.head q :: acc)

        loop q []

    let input = [ 1..50 ]
    drain (PhysicistsQueue.ofList input) |> should equal input

[<Fact>]
let ``real-time queue preserves FIFO order`` () =
    let drain q =
        let rec loop q acc =
            if RealTimeQueue.isEmpty q then List.rev acc
            else loop (RealTimeQueue.tail q) (RealTimeQueue.head q :: acc)

        loop q []

    let input = shuffled 100
    drain (RealTimeQueue.ofList input) |> should equal input

[<Fact>]
let ``interleaved snoc and tail behave like a FIFO`` () =
    // Enqueue 1..10, dequeue 3, enqueue 11..13, then drain fully.
    let afterEnqueue = [ 1..10 ] |> List.fold (fun q x -> BankersQueue.snoc x q) BankersQueue.empty
    let afterThreeTails = afterEnqueue |> BankersQueue.tail |> BankersQueue.tail |> BankersQueue.tail
    let afterMore = [ 11; 12; 13 ] |> List.fold (fun q x -> BankersQueue.snoc x q) afterThreeTails
    BankersQueue.toList afterMore |> should equal [ 4; 5; 6; 7; 8; 9; 10; 11; 12; 13 ]

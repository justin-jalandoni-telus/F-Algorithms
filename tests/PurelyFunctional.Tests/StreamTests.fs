module PurelyFunctional.Tests.StreamTests

open Xunit
open FsUnit.Xunit
open PurelyFunctional

[<Fact>]
let ``round-trips through a list`` () =
    Stream.ofList [ 1; 2; 3; 4 ] |> Stream.toList |> should equal [ 1; 2; 3; 4 ]

[<Fact>]
let ``append concatenates`` () =
    Stream.append (Stream.ofList [ 1; 2 ]) (Stream.ofList [ 3; 4 ])
    |> Stream.toList
    |> should equal [ 1; 2; 3; 4 ]

[<Fact>]
let ``reverse reverses`` () =
    Stream.ofList [ 1; 2; 3 ] |> Stream.reverse |> Stream.toList |> should equal [ 3; 2; 1 ]

[<Fact>]
let ``take and drop slice the stream`` () =
    let s = Stream.ofList [ 1..10 ]
    Stream.take 3 s |> Stream.toList |> should equal [ 1; 2; 3 ]
    Stream.drop 7 s |> Stream.toList |> should equal [ 8; 9; 10 ]

[<Fact>]
let ``take is lazy over an infinite stream`` () =
    let rec from n =
        Stream.cons n (Stream.suspend (fun () -> Stream.force (from (n + 1))))

    from 1 |> Stream.take 5 |> Stream.toList |> should equal [ 1; 2; 3; 4; 5 ]

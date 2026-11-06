module PurelyFunctional.Tests.RandomAccessListTests

open Xunit
open FsUnit.Xunit
open PurelyFunctional

module Bral = BinaryRandomAccessList
module Sbral = SkewBinaryRandomAccessList

[<Fact>]
let ``binary RAL round-trips through a list`` () =
    Bral.ofList [ 0..20 ] |> Bral.toList |> should equal [ 0..20 ]

[<Fact>]
let ``binary RAL looks up every index`` () =
    let ral = Bral.ofList [ 0..20 ]
    for i in 0..20 do
        Bral.lookup i ral |> should equal i

[<Fact>]
let ``binary RAL updates one index`` () =
    let ral = Bral.ofList [ 0..9 ] |> Bral.update 3 100
    Bral.lookup 3 ral |> should equal 100
    Bral.lookup 4 ral |> should equal 4
    Bral.toList ral |> should equal [ 0; 1; 2; 100; 4; 5; 6; 7; 8; 9 ]

[<Fact>]
let ``binary RAL cons, head and tail`` () =
    let ral = Bral.cons 99 (Bral.ofList [ 1; 2; 3 ])
    Bral.head ral |> should equal 99
    Bral.tail ral |> Bral.toList |> should equal [ 1; 2; 3 ]

[<Fact>]
let ``binary RAL rejects out-of-range lookup`` () =
    let ral = Bral.ofList [ 0..4 ]
    (fun () -> Bral.lookup 99 ral |> ignore)
    |> should throw typeof<System.IndexOutOfRangeException>

[<Fact>]
let ``skew binary RAL round-trips through a list`` () =
    Sbral.ofList [ 0..20 ] |> Sbral.toList |> should equal [ 0..20 ]

[<Fact>]
let ``skew binary RAL looks up every index`` () =
    let ral = Sbral.ofList [ 0..20 ]
    for i in 0..20 do
        Sbral.lookup i ral |> should equal i

[<Fact>]
let ``skew binary RAL updates one index`` () =
    let ral = Sbral.ofList [ 0..9 ] |> Sbral.update 3 100
    Sbral.lookup 3 ral |> should equal 100
    Sbral.lookup 4 ral |> should equal 4

[<Fact>]
let ``skew binary RAL cons, head and tail`` () =
    let ral = Sbral.cons 99 (Sbral.ofList [ 1; 2; 3 ])
    Sbral.head ral |> should equal 99
    Sbral.tail ral |> Sbral.toList |> should equal [ 1; 2; 3 ]

[<Fact>]
let ``skew binary RAL rejects out-of-range lookup`` () =
    let ral = Sbral.ofList [ 0..4 ]
    (fun () -> Sbral.lookup 99 ral |> ignore)
    |> should throw typeof<System.IndexOutOfRangeException>

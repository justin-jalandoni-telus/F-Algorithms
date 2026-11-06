module Algorithms.Tests.CountMinSketchTests

open Xunit
open Algorithms

[<Fact>]
let ``estimate never undercounts`` () =
    let sketch =
        CountMinSketch.create Hash.string 0.001 0.001
        |> CountMinSketch.addAll (seq { for _ in 1..50 -> "apple" })
        |> CountMinSketch.addAll (seq { for _ in 1..10 -> "pear" })

    Assert.True(CountMinSketch.estimate "apple" sketch >= 50)
    Assert.True(CountMinSketch.estimate "pear" sketch >= 10)

[<Fact>]
let ``absent item estimate stays small`` () =
    let sketch =
        CountMinSketch.create Hash.string 0.001 0.001
        |> CountMinSketch.addAll (seq { for _ in 1..100 -> "apple" })

    Assert.True(CountMinSketch.estimate "banana" sketch <= 2)

[<Fact>]
let ``add does not mutate the original sketch`` () =
    let original = CountMinSketch.create Hash.string 0.01 0.01
    let updated = original |> CountMinSketch.add "x"
    Assert.Equal(0, CountMinSketch.estimate "x" original)
    Assert.Equal(1, CountMinSketch.estimate "x" updated)

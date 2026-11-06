module Algorithms.Tests.HyperLogLogTests

open Xunit
open Algorithms

let private relativeError (estimate: float) (actual: int) =
    abs (estimate - float actual) / float actual

[<Fact>]
let ``estimates distinct count within error bound`` () =
    let distinct = 5000
    let sketch = HyperLogLog.create Hash.int32 14 |> HyperLogLog.addAll (seq { 1..distinct })
    let error = relativeError (HyperLogLog.count sketch) distinct
    Assert.True(error < 0.05, $"relative error was {error}")

[<Fact>]
let ``duplicates do not inflate the estimate`` () =
    let sketch =
        HyperLogLog.create Hash.int32 14
        |> HyperLogLog.addAll (seq { for _ in 1..1000 -> 42 })

    Assert.True(HyperLogLog.count sketch < 5.0)

[<Fact>]
let ``merge unions two sketches`` () =
    let left = HyperLogLog.create Hash.int32 14 |> HyperLogLog.addAll (seq { 1..2000 })
    let right = HyperLogLog.create Hash.int32 14 |> HyperLogLog.addAll (seq { 1500..3500 })
    let error = relativeError (HyperLogLog.count (HyperLogLog.merge left right)) 3500
    Assert.True(error < 0.05, $"relative error was {error}")

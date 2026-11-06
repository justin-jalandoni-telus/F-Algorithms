module Algorithms.Tests.BloomFilterTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``no false negatives for added items`` () =
    let items = [ "alpha"; "beta"; "gamma"; "delta" ]
    let filter = BloomFilter.create Hash.string 100 0.01 |> BloomFilter.addAll items
    items |> List.iter (fun i -> BloomFilter.mightContain i filter |> should equal true)

[<Fact>]
let ``absent item is reported missing`` () =
    let filter = BloomFilter.create Hash.string 100 0.001 |> BloomFilter.add "present"
    BloomFilter.mightContain "definitely-absent" filter |> should equal false

[<Fact>]
let ``add does not mutate the original filter`` () =
    let original = BloomFilter.create Hash.string 100 0.01
    let updated = BloomFilter.add "x" original
    BloomFilter.mightContain "x" original |> should equal false
    BloomFilter.mightContain "x" updated |> should equal true

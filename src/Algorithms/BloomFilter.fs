namespace Algorithms

/// An immutable Bloom filter: a probabilistic set with no false negatives
/// and a tunable false-positive rate.
type BloomFilter<'T> =
    { Bits: bool[]
      HashCount: int
      Hash: Hasher<'T> }

[<RequireQualifiedAccess>]
module BloomFilter =

    let private optimalBitCount expectedItems falsePositiveRate =
        let n = float (max 1 expectedItems)
        let m = -(n * log falsePositiveRate) / ((log 2.0) ** 2.0)
        max 1 (int (ceil m))

    let private optimalHashCount bitCount expectedItems =
        let m = float bitCount
        let n = float (max 1 expectedItems)
        max 1 (int (round (m / n * log 2.0)))

    /// Creates a filter sized for <paramref name="expectedItems"/> at the
    /// target <paramref name="falsePositiveRate"/> (e.g. 0.01 for 1%).
    let create (hash: Hasher<'T>) expectedItems falsePositiveRate : BloomFilter<'T> =
        let bitCount = optimalBitCount expectedItems falsePositiveRate
        { Bits = Array.zeroCreate bitCount
          HashCount = optimalHashCount bitCount expectedItems
          Hash = hash }

    let private setBits (filter: BloomFilter<'T>) (bits: bool[]) item =
        let pair = item |> filter.Hash |> Hash.toPair
        for i in 0 .. filter.HashCount - 1 do
            bits.[Hash.indexAt pair i bits.Length] <- true

    /// Returns a new filter containing <paramref name="item"/>.
    let add item (filter: BloomFilter<'T>) : BloomFilter<'T> =
        let bits = Array.copy filter.Bits
        setBits filter bits item
        { filter with Bits = bits }

    /// Adds many items, cloning the backing array only once.
    let addAll items (filter: BloomFilter<'T>) : BloomFilter<'T> =
        let bits = Array.copy filter.Bits
        items |> Seq.iter (setBits filter bits)
        { filter with Bits = bits }

    /// <c>false</c> means definitely absent; <c>true</c> means possibly present.
    let mightContain item (filter: BloomFilter<'T>) : bool =
        let pair = item |> filter.Hash |> Hash.toPair
        seq { 0 .. filter.HashCount - 1 }
        |> Seq.forall (fun i -> filter.Bits.[Hash.indexAt pair i filter.Bits.Length])

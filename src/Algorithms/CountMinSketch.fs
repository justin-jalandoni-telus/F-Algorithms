namespace Algorithms

/// An immutable Count-Min sketch for approximate frequency counts. Estimates
/// never undercount; they may overcount with bounded probability.
type CountMinSketch<'T> =
    { Counts: int[][]
      Width: int
      Depth: int
      Hash: Hasher<'T> }

[<RequireQualifiedAccess>]
module CountMinSketch =

    /// Creates a sketch. <paramref name="epsilon"/> bounds the overestimate
    /// (as a fraction of the total count) and <paramref name="delta"/> bounds
    /// the probability of exceeding it.
    let create (hash: Hasher<'T>) epsilon delta : CountMinSketch<'T> =
        let width = max 1 (int (ceil (System.Math.E / epsilon)))
        let depth = max 1 (int (ceil (log (1.0 / delta))))
        { Counts = Array.init depth (fun _ -> Array.zeroCreate width)
          Width = width
          Depth = depth
          Hash = hash }

    let private columnOf width (hash: Hasher<'T>) item =
        let pair = item |> hash |> Hash.toPair
        fun row -> Hash.indexAt pair row width

    let private bump (sketch: CountMinSketch<'T>) (counts: int[][]) item =
        let column = columnOf sketch.Width sketch.Hash item
        for row in 0 .. counts.Length - 1 do
            let col = column row
            counts.[row].[col] <- counts.[row].[col] + 1

    let private clone (counts: int[][]) = counts |> Array.map Array.copy

    /// Returns a new sketch recording one occurrence of <paramref name="item"/>.
    let add item (sketch: CountMinSketch<'T>) : CountMinSketch<'T> =
        let counts = clone sketch.Counts
        bump sketch counts item
        { sketch with Counts = counts }

    /// Records many occurrences, cloning the counters only once.
    let addAll items (sketch: CountMinSketch<'T>) : CountMinSketch<'T> =
        let counts = clone sketch.Counts
        items |> Seq.iter (bump sketch counts)
        { sketch with Counts = counts }

    /// The approximate number of times <paramref name="item"/> has been added.
    let estimate item (sketch: CountMinSketch<'T>) : int =
        let column = columnOf sketch.Width sketch.Hash item
        seq { 0 .. sketch.Depth - 1 }
        |> Seq.map (fun row -> sketch.Counts.[row].[column row])
        |> Seq.min

namespace Algorithms

/// An immutable HyperLogLog sketch for approximate distinct-count queries.
type HyperLogLog<'T> =
    { Registers: byte[]
      Precision: int
      Hash: Hasher<'T> }

[<RequireQualifiedAccess>]
module HyperLogLog =

    let private alpha m =
        match m with
        | 16 -> 0.673
        | 32 -> 0.697
        | 64 -> 0.709
        | _ -> 0.7213 / (1.0 + 1.079 / float m)

    /// Creates a sketch. <paramref name="precision"/> (4..16) trades memory
    /// for accuracy; the standard error is roughly 1.04 / sqrt(2^precision).
    let create (hash: Hasher<'T>) precision : HyperLogLog<'T> =
        if precision < 4 || precision > 16 then
            invalidArg (nameof precision) "precision must be between 4 and 16"
        { Registers = Array.zeroCreate (1 <<< precision)
          Precision = precision
          Hash = hash }

    let private leadingZeros (value: uint64) (width: int) =
        let rec loop mask count =
            if mask = 0UL then count
            elif value &&& mask <> 0UL then count
            else loop (mask >>> 1) (count + 1)
        loop (1UL <<< (width - 1)) 0

    let private observe (sketch: HyperLogLog<'T>) (registers: byte[]) item =
        let h = sketch.Hash item
        let index = int (h >>> (64 - sketch.Precision))
        let remainingWidth = 64 - sketch.Precision
        let remaining = h &&& ((1UL <<< remainingWidth) - 1UL)
        let rank = byte (leadingZeros remaining remainingWidth + 1)
        if rank > registers.[index] then
            registers.[index] <- rank

    /// Returns a new sketch that has observed <paramref name="item"/>.
    let add item (sketch: HyperLogLog<'T>) : HyperLogLog<'T> =
        let registers = Array.copy sketch.Registers
        observe sketch registers item
        { sketch with Registers = registers }

    /// Observes many items, cloning the registers only once.
    let addAll items (sketch: HyperLogLog<'T>) : HyperLogLog<'T> =
        let registers = Array.copy sketch.Registers
        items |> Seq.iter (observe sketch registers)
        { sketch with Registers = registers }

    /// The estimated number of distinct items observed.
    let count (sketch: HyperLogLog<'T>) : float =
        let m = float sketch.Registers.Length
        let harmonic = sketch.Registers |> Array.sumBy (fun r -> 2.0 ** (- float r))
        let raw = alpha (int m) * m * m / harmonic
        let zeros = sketch.Registers |> Array.filter ((=) 0uy) |> Array.length
        if raw <= 2.5 * m && zeros <> 0 then
            m * log (m / float zeros)
        else
            raw

    /// The element-wise union of two sketches sharing the same precision.
    let merge (a: HyperLogLog<'T>) (b: HyperLogLog<'T>) : HyperLogLog<'T> =
        if a.Precision <> b.Precision then
            invalidArg (nameof b) "sketches must share the same precision"
        { a with Registers = Array.map2 max a.Registers b.Registers }

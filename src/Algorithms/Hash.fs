namespace Algorithms

/// A hash function mapping a value to a 64-bit fingerprint.
type Hasher<'T> = 'T -> uint64

/// 64-bit FNV-1a hashing plus double-hashing helpers shared by the
/// probabilistic data structures.
[<RequireQualifiedAccess>]
module Hash =

    [<Literal>]
    let private FnvOffsetBasis = 1469598103934665603UL

    [<Literal>]
    let private FnvPrime = 1099511628211UL

    /// Hashes a raw byte span with 64-bit FNV-1a.
    let bytes : Hasher<byte[]> =
        fun data ->
            let mutable hash = FnvOffsetBasis
            for b in data do
                hash <- (hash ^^^ uint64 b) * FnvPrime
            hash

    /// Hashes a string via its UTF-8 encoding.
    let string : Hasher<string> =
        fun value -> value |> System.Text.Encoding.UTF8.GetBytes |> bytes

    /// Hashes a 32-bit integer.
    let int32 : Hasher<int> =
        fun value -> value |> System.BitConverter.GetBytes |> bytes

    /// Hashes a 64-bit integer.
    let int64 : Hasher<int64> =
        fun value -> value |> System.BitConverter.GetBytes |> bytes

    /// Splits a 64-bit hash into two 32-bit hashes for the
    /// Kirsch-Mitzenmacher double-hashing scheme: g_i = h1 + i * h2.
    let toPair (hash: uint64) : uint64 * uint64 =
        let h1 = hash &&& 0xFFFFFFFFUL
        let h2 = (hash >>> 32) &&& 0xFFFFFFFFUL
        h1, (if h2 = 0UL then 0x9E3779B9UL else h2)

    /// The i-th derived index within <c>[0, modulus)</c> using double hashing,
    /// where <paramref name="pair"/> is a split hash from <see cref="toPair"/>.
    let indexAt (pair: uint64 * uint64) (i: int) (modulus: int) : int =
        let h1, h2 = pair
        int ((h1 + uint64 i * h2) % uint64 modulus)

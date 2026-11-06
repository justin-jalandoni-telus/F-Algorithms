namespace PurelyFunctional

/// A memoized lazy stream (Okasaki, Chapter 4). The whole spine is suspended,
/// so forcing is incremental and shared between all observers of a suspension.
type Stream<'T> = Susp of Lazy<StreamCell<'T>>

/// The forced form of a <see cref="Stream{T}"/>: either empty or a head value
/// followed by the (still suspended) tail.
and StreamCell<'T> =
    | SNil
    | SCons of 'T * Stream<'T>

[<RequireQualifiedAccess>]
module Stream =

    /// Forces a stream, yielding its head/tail cell.
    let force (Susp thunk) = thunk.Value

    /// The empty stream.
    let empty<'T> : Stream<'T> = Susp(lazy SNil)

    /// Builds a stream from a thunk producing the next cell (a suspension).
    let suspend (thunk: unit -> StreamCell<'T>) : Stream<'T> = Susp(lazy (thunk ()))

    /// Prepends a value to a stream (the head is memoized, not recomputed).
    let cons x s : Stream<'T> = Susp(lazy SCons(x, s))

    /// Whether the stream is empty.
    let isEmpty s =
        match force s with
        | SNil -> true
        | _ -> false

    /// The first element, or a failure for the empty stream.
    let head s =
        match force s with
        | SNil -> failwith "head: empty stream"
        | SCons(x, _) -> x

    /// The stream without its first element.
    let tail s =
        match force s with
        | SNil -> failwith "tail: empty stream"
        | SCons(_, t) -> t

    /// Lazily appends two streams.
    let rec append a b : Stream<'T> =
        suspend (fun () ->
            match force a with
            | SNil -> force b
            | SCons(x, s) -> SCons(x, append s b))

    /// Reverses a stream (as a single suspension).
    let reverse s : Stream<'T> =
        let rec loop acc s =
            match force s with
            | SNil -> acc
            | SCons(x, rest) -> loop (cons x acc) rest
        suspend (fun () -> force (loop empty s))

    /// The first <paramref name="n"/> elements.
    let rec take n s : Stream<'T> =
        suspend (fun () ->
            if n <= 0 then
                SNil
            else
                match force s with
                | SNil -> SNil
                | SCons(x, rest) -> SCons(x, take (n - 1) rest))

    /// The stream with its first <paramref name="n"/> elements removed.
    let rec drop n s : Stream<'T> =
        if n <= 0 then
            s
        else
            suspend (fun () ->
                match force s with
                | SNil -> SNil
                | SCons(_, rest) -> force (drop (n - 1) rest))

    /// The number of elements (forces the whole stream).
    let length s =
        let rec loop n s =
            match force s with
            | SNil -> n
            | SCons(_, rest) -> loop (n + 1) rest
        loop 0 s

    /// Builds a stream from a list.
    let ofList xs : Stream<'T> = List.foldBack cons xs empty

    /// Materializes a stream into a list (forces the whole stream).
    let rec toList s =
        match force s with
        | SNil -> []
        | SCons(x, rest) -> x :: toList rest

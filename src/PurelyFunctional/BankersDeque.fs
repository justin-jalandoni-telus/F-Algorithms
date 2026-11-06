namespace PurelyFunctional

/// Okasaki's banker's deque (Chapter 8): a double-ended queue keeping the front
/// and rear streams balanced within a constant factor <c>c</c>, so both ends
/// support amortized O(1) access.
module BankersDeque =

    [<Literal>]
    let private C = 2

    /// A deque tracking the lengths of its front and rear streams.
    type Deque<'T> =
        { LenF: int
          Front: Stream<'T>
          LenR: int
          Rear: Stream<'T> }

    /// The empty deque.
    let empty<'T> : Deque<'T> =
        { LenF = 0; Front = Stream.empty; LenR = 0; Rear = Stream.empty }

    /// Whether the deque is empty.
    let isEmpty q = q.LenF + q.LenR = 0

    let private check q =
        if q.LenF > C * q.LenR + 1 then
            let i = (q.LenF + q.LenR) / 2
            let j = q.LenF + q.LenR - i
            { LenF = i
              Front = Stream.take i q.Front
              LenR = j
              Rear = Stream.append q.Rear (Stream.reverse (Stream.drop i q.Front)) }
        elif q.LenR > C * q.LenF + 1 then
            let j = (q.LenF + q.LenR) / 2
            let i = q.LenF + q.LenR - j
            { LenF = i
              Front = Stream.append q.Front (Stream.reverse (Stream.drop j q.Rear))
              LenR = j
              Rear = Stream.take j q.Rear }
        else
            q

    /// Prepends a value to the front.
    let cons x q =
        check { q with LenF = q.LenF + 1; Front = Stream.cons x q.Front }

    /// Appends a value to the rear.
    let snoc x q =
        check { q with LenR = q.LenR + 1; Rear = Stream.cons x q.Rear }

    /// The front element.
    let head q =
        match Stream.force q.Front with
        | SCons(x, _) -> x
        | SNil ->
            match Stream.force q.Rear with
            | SCons(x, _) -> x
            | SNil -> failwith "head: empty deque"

    /// The rear element.
    let last q =
        match Stream.force q.Rear with
        | SCons(x, _) -> x
        | SNil ->
            match Stream.force q.Front with
            | SCons(x, _) -> x
            | SNil -> failwith "last: empty deque"

    /// The deque without its front element.
    let tail q =
        match Stream.force q.Front with
        | SCons(_, f) -> check { q with LenF = q.LenF - 1; Front = f }
        | SNil ->
            match Stream.force q.Rear with
            | SNil -> failwith "tail: empty deque"
            | SCons _ -> empty

    /// The deque without its rear element.
    let init q =
        match Stream.force q.Rear with
        | SCons(_, r) -> check { q with LenR = q.LenR - 1; Rear = r }
        | SNil ->
            match Stream.force q.Front with
            | SNil -> failwith "init: empty deque"
            | SCons _ -> empty

    /// Builds a deque from a list (front to back).
    let ofList xs : Deque<'T> = List.fold (fun q x -> snoc x q) empty xs

    /// Materializes the deque front-to-back.
    let toList q = Stream.toList q.Front @ List.rev (Stream.toList q.Rear)

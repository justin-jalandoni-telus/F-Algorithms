namespace PurelyFunctional

/// Okasaki's banker's queue (Chapter 6): front and rear are lazy streams, and
/// the rear is lazily reversed onto the front, giving amortized O(1) bounds
/// that survive persistent (multi-version) use.
module BankersQueue =

    /// A queue tracking the lengths of its front and rear streams.
    type Queue<'T> =
        { LenF: int
          Front: Stream<'T>
          LenR: int
          Rear: Stream<'T> }

    /// The empty queue.
    let empty<'T> : Queue<'T> =
        { LenF = 0; Front = Stream.empty; LenR = 0; Rear = Stream.empty }

    /// Whether the queue is empty.
    let isEmpty q = q.LenF = 0

    let private check q =
        if q.LenR <= q.LenF then
            q
        else
            { LenF = q.LenF + q.LenR
              Front = Stream.append q.Front (Stream.reverse q.Rear)
              LenR = 0
              Rear = Stream.empty }

    /// Enqueues a value at the rear.
    let snoc x q =
        check { q with LenR = q.LenR + 1; Rear = Stream.cons x q.Rear }

    /// The front element.
    let head q = Stream.head q.Front

    /// The queue without its front element.
    let tail q =
        match Stream.force q.Front with
        | SNil -> failwith "tail: empty queue"
        | SCons(_, f) -> check { q with LenF = q.LenF - 1; Front = f }

    /// Builds a queue from a list (front to back).
    let ofList xs : Queue<'T> = List.fold (fun q x -> snoc x q) empty xs

    /// Materializes the queue front-to-back.
    let toList q = Stream.toList q.Front @ List.rev (Stream.toList q.Rear)

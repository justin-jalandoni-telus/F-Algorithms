namespace PurelyFunctional

/// Okasaki's real-time queue (Chapter 7): streams plus an explicit schedule
/// that forces the incremental rotation one step per operation, turning the
/// amortized bounds into worst-case O(1).
module RealTimeQueue =

    /// A queue with a front stream, a reversed rear list, and a schedule
    /// (a suffix of the front still to be forced).
    type Queue<'T> =
        { Front: Stream<'T>
          Rear: 'T list
          Schedule: Stream<'T> }

    /// The empty queue.
    let empty<'T> : Queue<'T> =
        { Front = Stream.empty; Rear = []; Schedule = Stream.empty }

    /// Whether the queue is empty.
    let isEmpty q = Stream.isEmpty q.Front

    let rec private rotate front rear acc : Stream<'T> =
        match rear with
        | [] -> failwith "rotate: invariant violated"
        | y :: ys ->
            Stream.suspend (fun () ->
                match Stream.force front with
                | SNil -> SCons(y, acc)
                | SCons(x, xs) -> SCons(x, rotate xs ys (Stream.cons y acc)))

    let private exec q =
        match Stream.force q.Schedule with
        | SCons(_, s) -> { q with Schedule = s }
        | SNil ->
            let front = rotate q.Front q.Rear Stream.empty
            { Front = front; Rear = []; Schedule = front }

    /// Enqueues a value at the rear.
    let snoc x q = exec { q with Rear = x :: q.Rear }

    /// The front element.
    let head q = Stream.head q.Front

    /// The queue without its front element.
    let tail q =
        match Stream.force q.Front with
        | SNil -> failwith "tail: empty queue"
        | SCons(_, f) -> exec { q with Front = f }

    /// Builds a queue from a list (front to back).
    let ofList xs : Queue<'T> = List.fold (fun q x -> snoc x q) empty xs

    /// Materializes the queue front-to-back.
    let toList q = Stream.toList q.Front @ List.rev q.Rear

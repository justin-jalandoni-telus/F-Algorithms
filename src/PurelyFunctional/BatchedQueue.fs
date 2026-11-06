namespace PurelyFunctional

/// Okasaki's batched queue (Chapter 5): a front list and a reversed rear list,
/// giving amortized O(1) operations.
module BatchedQueue =

    /// A queue with a front list and a reversed rear list.
    type Queue<'T> = { Front: 'T list; Rear: 'T list }

    /// The empty queue.
    let empty<'T> : Queue<'T> = { Front = []; Rear = [] }

    /// Whether the queue is empty.
    let isEmpty q = List.isEmpty q.Front

    let private check q =
        match q.Front with
        | [] -> { Front = List.rev q.Rear; Rear = [] }
        | _ -> q

    /// Enqueues a value at the rear.
    let snoc x q = check { q with Rear = x :: q.Rear }

    /// The front element.
    let head q =
        match q.Front with
        | [] -> failwith "head: empty queue"
        | x :: _ -> x

    /// The queue without its front element.
    let tail q =
        match q.Front with
        | [] -> failwith "tail: empty queue"
        | _ :: f -> check { q with Front = f }

    /// Builds a queue from a list (front to back).
    let ofList xs : Queue<'T> = List.fold (fun q x -> snoc x q) empty xs

    /// Materializes the queue front-to-back.
    let toList q = q.Front @ List.rev q.Rear

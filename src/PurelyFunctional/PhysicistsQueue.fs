namespace PurelyFunctional

/// Okasaki's physicist's queue (Chapter 6): a suspended front list with a
/// strict working prefix for O(1) <c>head</c>, and a reversed rear list.
module PhysicistsQueue =

    /// A queue with a strict working prefix, a suspended front, and a rear list.
    type Queue<'T> =
        { Working: 'T list
          LenF: int
          Front: Lazy<'T list>
          LenR: int
          Rear: 'T list }

    /// The empty queue.
    let empty<'T> : Queue<'T> =
        { Working = []; LenF = 0; Front = lazy []; LenR = 0; Rear = [] }

    /// Whether the queue is empty.
    let isEmpty q = q.LenF = 0

    let private checkWorking q =
        match q.Working with
        | [] -> { q with Working = q.Front.Value }
        | _ -> q

    let private check q =
        if q.LenR <= q.LenF then
            checkWorking q
        else
            let front = q.Front.Value
            checkWorking
                { Working = front
                  LenF = q.LenF + q.LenR
                  Front = lazy (front @ List.rev q.Rear)
                  LenR = 0
                  Rear = [] }

    /// Enqueues a value at the rear.
    let snoc x q =
        check { q with LenR = q.LenR + 1; Rear = x :: q.Rear }

    /// The front element.
    let head q =
        match q.Working with
        | [] -> failwith "head: empty queue"
        | x :: _ -> x

    /// The queue without its front element.
    let tail q =
        match q.Working with
        | [] -> failwith "tail: empty queue"
        | _ :: rest ->
            check
                { q with
                    Working = rest
                    LenF = q.LenF - 1
                    Front = lazy (List.tail q.Front.Value) }

    /// Builds a queue from a list (front to back).
    let ofList xs : Queue<'T> = List.fold (fun q x -> snoc x q) empty xs

    /// Materializes the queue front-to-back.
    let toList q = q.Front.Value @ List.rev q.Rear

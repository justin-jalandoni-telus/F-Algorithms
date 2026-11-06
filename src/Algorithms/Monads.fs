namespace Algorithms

/// A computation that either succeeded with a value or failed with an
/// exception - a total alternative to throwing.
type Try<'T> =
    | Success of 'T
    | Failure of exn

[<RequireQualifiedAccess>]
module Try =

    /// Lifts a value into a successful computation.
    let retn value = Success value

    /// Runs <paramref name="operation"/>, capturing any exception as a failure.
    let run operation =
        try Success(operation ())
        with ex -> Failure ex

    /// Functor map: transforms a success, capturing exceptions from <paramref name="f"/>.
    let map f =
        function
        | Success x -> (try Success(f x) with ex -> Failure ex)
        | Failure e -> Failure e

    /// Monadic bind: sequences dependent computations.
    let bind f =
        function
        | Success x -> (try f x with ex -> Failure ex)
        | Failure e -> Failure e

    /// Applicative apply: combines a wrapped function with a wrapped argument.
    let apply fTry xTry =
        match fTry, xTry with
        | Success f, Success x -> (try Success(f x) with ex -> Failure ex)
        | Failure e, _ -> Failure e
        | _, Failure e -> Failure e

    /// Replaces a failure with the result of <paramref name="handler"/>.
    let recover handler =
        function
        | Success x -> Success x
        | Failure e -> (try Success(handler e) with ex -> Failure ex)

    /// The contained value, or <paramref name="fallback"/> on failure.
    let defaultValue fallback =
        function
        | Success x -> x
        | Failure _ -> fallback

    /// Converts to a <see cref="Result{T, E}"/>.
    let toResult =
        function
        | Success x -> Ok x
        | Failure e -> Error e

    /// Converts to an option, discarding the exception.
    let toOption =
        function
        | Success x -> Some x
        | Failure _ -> None

/// Computation-expression builder for <see cref="Try{T}"/>.
type TryBuilder() =
    member _.Return(x) = Try.retn x
    member _.ReturnFrom(m: Try<'T>) = m
    member _.Bind(m, f) = Try.bind f m
    member _.Zero() = Try.retn ()
    member _.Delay(f: unit -> Try<'T>) = f
    member _.Run(f: unit -> Try<'T>) = Try.bind id (Try.run f)

[<AutoOpen>]
module TryExpression =
    /// Builds exception-capturing computations with <c>let!</c> / <c>return</c>.
    let tryResult = TryBuilder()


/// The Reader monad: a computation awaiting a shared environment.
type Reader<'Env, 'T> = Reader of ('Env -> 'T)

[<RequireQualifiedAccess>]
module Reader =
    /// Runs the computation against an environment.
    let run env (Reader f) = f env

    /// Lifts a value, ignoring the environment.
    let retn value = Reader(fun _ -> value)

    /// Functor map over the produced value.
    let map f (Reader g) = Reader(fun env -> f (g env))

    /// Monadic bind threading the same environment.
    let bind f (Reader g) = Reader(fun env -> run env (f (g env)))

    /// Applicative apply sharing one environment.
    let apply (Reader f) (Reader g) = Reader(fun env -> (f env) (g env))

    /// Retrieves the environment itself.
    let ask<'Env> : Reader<'Env, 'Env> = Reader id

    /// Projects a value out of the environment.
    let asks (f: 'Env -> 'T) = Reader f

/// Computation-expression builder for <see cref="Reader{Env, T}"/>.
type ReaderBuilder() =
    member _.Return(x) = Reader.retn x
    member _.ReturnFrom(m: Reader<'E, 'T>) = m
    member _.Bind(m, f) = Reader.bind f m

[<AutoOpen>]
module ReaderExpression =
    /// Builds environment-reading computations.
    let reader = ReaderBuilder()


/// The State monad: a function threading mutable-looking state purely.
type State<'S, 'T> = State of ('S -> 'T * 'S)

[<RequireQualifiedAccess>]
module State =
    /// Runs the computation, returning the value and final state.
    let run state (State f) = f state

    /// Lifts a value, leaving the state unchanged.
    let retn value = State(fun s -> value, s)

    /// Monadic bind threading the evolving state.
    let bind f (State g) =
        State(fun s ->
            let value, s' = g s
            run s' (f value))

    /// Functor map over the produced value.
    let map f m = bind (f >> retn) m

    /// Applicative apply threading state left to right.
    let apply fState xState = fState |> bind (fun f -> xState |> map f)

    /// Reads the current state as the value.
    let get = State(fun s -> s, s)

    /// Replaces the current state.
    let put newState = State(fun _ -> (), newState)

    /// Transforms the current state with <paramref name="f"/>.
    let modify f = State(fun s -> (), f s)

    /// Runs the computation and returns only the value.
    let eval state m = run state m |> fst

    /// Runs the computation and returns only the final state.
    let exec state m = run state m |> snd

/// Computation-expression builder for <see cref="State{S, T}"/>.
type StateBuilder() =
    member _.Return(x) = State.retn x
    member _.ReturnFrom(m: State<'S, 'T>) = m
    member _.Bind(m, f) = State.bind f m

[<AutoOpen>]
module StateExpression =
    /// Builds stateful computations.
    let state = StateBuilder()


/// Functor / applicative / monad helpers for <see cref="Option{T}"/>.
[<RequireQualifiedAccess>]
module OptionM =
    /// Functor map.
    let map = Option.map
    /// Applicative pure.
    let retn value = Some value
    /// Applicative apply.
    let apply fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some(f x)
        | _ -> None
    /// Monadic bind.
    let bind = Option.bind


/// Functor / applicative / monad helpers for <see cref="Result{T, E}"/>.
[<RequireQualifiedAccess>]
module ResultM =
    /// Functor map over the success value.
    let map = Result.map
    /// Map over the error value.
    let mapError = Result.mapError
    /// Applicative pure.
    let retn value : Result<'T, 'E> = Ok value
    /// Applicative apply, returning the first error encountered.
    let apply fRes xRes =
        match fRes, xRes with
        | Ok f, Ok x -> Ok(f x)
        | Error e, _ -> Error e
        | _, Error e -> Error e
    /// Monadic bind.
    let bind = Result.bind

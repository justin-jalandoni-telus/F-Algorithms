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

    /// Applicative lift of a binary function over two computations.
    let map2 f a b = apply (map f a) b

    /// Applicative lift of a ternary function over three computations.
    let map3 f a b c = apply (apply (map f a) b) c

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
    /// Applicative lift of a binary function over two options.
    let map2 f a b = apply (map f a) b
    /// Applicative lift of a ternary function over three options.
    let map3 f a b c = apply (apply (map f a) b) c
    /// Applies <paramref name="f"/> to each element, collecting the results
    /// when all are <c>Some</c>; yields <c>None</c> if any is <c>None</c>.
    let traverse f list =
        List.foldBack (fun x state -> map2 (fun y ys -> y :: ys) (f x) state) list (Some [])
    /// Turns a list of options into an option of the list of their values.
    let sequence list = traverse id list


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
    /// Applicative lift of a binary function, returning the first error.
    let map2 f a b = apply (map f a) b
    /// Applicative lift of a ternary function, returning the first error.
    let map3 f a b c = apply (apply (map f a) b) c
    /// Applies <paramref name="f"/> to each element, collecting the results
    /// when all are <c>Ok</c>; yields the first <c>Error</c> otherwise.
    let traverse f list =
        List.foldBack (fun x state -> map2 (fun y ys -> y :: ys) (f x) state) list (Ok [])
    /// Turns a list of results into a result of the list of their values.
    let sequence list = traverse id list


/// The Either monad: a value that is either <c>Left</c> (by convention an
/// error) or <c>Right</c> (by convention a success). A richer, explicitly
/// two-sided alternative to <see cref="Result{T, E}"/>, ported from the
/// <c>Either&lt;L, R&gt;</c> type in Enrico Buonanno's <i>Functional
/// Programming in C#</i>.
type Either<'L, 'R> =
    | Left of 'L
    | Right of 'R

[<RequireQualifiedAccess>]
module Either =
    /// Lifts a value into the <c>Right</c> (success) case.
    let retn value = Right value

    /// Case analysis: applies <paramref name="onLeft"/> to a <c>Left</c> or
    /// <paramref name="onRight"/> to a <c>Right</c>.
    let either onLeft onRight =
        function
        | Left l -> onLeft l
        | Right r -> onRight r

    /// Functor map over the <c>Right</c> value; a <c>Left</c> passes through.
    let map f =
        function
        | Left l -> Left l
        | Right r -> Right(f r)

    /// Maps over the <c>Left</c> value only.
    let mapLeft f =
        function
        | Left l -> Left(f l)
        | Right r -> Right r

    /// Bifunctor map: transforms whichever side is present.
    let bimap onLeft onRight =
        function
        | Left l -> Left(onLeft l)
        | Right r -> Right(onRight r)

    /// Monadic bind over the <c>Right</c> value; a <c>Left</c> short-circuits.
    let bind f =
        function
        | Left l -> Left l
        | Right r -> f r

    /// Applicative apply; the first <c>Left</c> encountered short-circuits.
    let apply fEither xEither =
        match fEither, xEither with
        | Left l, _ -> Left l
        | Right f, Right x -> Right(f x)
        | Right _, Left l -> Left l

    /// Applicative lift of a binary function; the first <c>Left</c> short-circuits.
    let map2 f a b = apply (map f a) b

    /// Applicative lift of a ternary function; the first <c>Left</c> short-circuits.
    let map3 f a b c = apply (apply (map f a) b) c

    /// The <c>Right</c> value, or <paramref name="fallback"/> when <c>Left</c>.
    let defaultValue fallback =
        function
        | Right r -> r
        | Left _ -> fallback

    /// The <c>Right</c> value, or <paramref name="fallback"/> applied to the
    /// <c>Left</c>.
    let defaultWith fallback =
        function
        | Right r -> r
        | Left l -> fallback l

    /// Converts to a <see cref="Result{R, L}"/> (<c>Right</c> to <c>Ok</c>,
    /// <c>Left</c> to <c>Error</c>).
    let toResult =
        function
        | Right r -> Ok r
        | Left l -> Error l

    /// Converts a <see cref="Result{T, E}"/> (<c>Ok</c> to <c>Right</c>,
    /// <c>Error</c> to <c>Left</c>).
    let ofResult =
        function
        | Ok r -> Right r
        | Error e -> Left e

    /// Converts to an option, discarding any <c>Left</c>.
    let toOption =
        function
        | Right r -> Some r
        | Left _ -> None

/// Computation-expression builder for <see cref="Either{L, R}"/>.
type EitherBuilder() =
    member _.Return(x) = Either.retn x
    member _.ReturnFrom(m: Either<'L, 'R>) = m
    member _.Bind(m, f) = Either.bind f m

[<AutoOpen>]
module EitherExpression =
    /// Builds short-circuiting <c>Left</c>/<c>Right</c> computations.
    let either = EitherBuilder()


/// A domain error carrying a human-readable message. Mirrors the <c>Error</c>
/// type accumulated by <see cref="Validation{T}"/> in <i>Functional Programming
/// in C#</i>.
type ValidationError =
    { Message: string }
    override this.ToString() = this.Message

[<RequireQualifiedAccess>]
module ValidationError =
    /// Creates an error from a message.
    let create message = { Message = message }

/// The Validation applicative: either <c>Valid</c> with a value or
/// <c>Invalid</c> with accumulated errors. Unlike <see cref="Result{T, E}"/>
/// and <see cref="Either{L, R}"/>, <see cref="Validation.apply"/> collects the
/// errors from <em>every</em> operand rather than stopping at the first, which
/// is what makes it ideal for form/record validation. Ported from the
/// <c>Validation&lt;T&gt;</c> type in <i>Functional Programming in C#</i>.
type Validation<'T> =
    | Valid of 'T
    | Invalid of ValidationError list

[<RequireQualifiedAccess>]
module Validation =
    /// Lifts a value into the <c>Valid</c> case.
    let retn value = Valid value

    /// Creates an <c>Invalid</c> from a list of errors.
    let fail errors = Invalid errors

    /// Creates an <c>Invalid</c> from a single error message.
    let failWith message = Invalid [ ValidationError.create message ]

    /// Case analysis: applies <paramref name="onInvalid"/> to the errors or
    /// <paramref name="onValid"/> to the value.
    let validation onInvalid onValid =
        function
        | Invalid errors -> onInvalid errors
        | Valid value -> onValid value

    /// Functor map over the <c>Valid</c> value; errors pass through.
    let map f =
        function
        | Valid value -> Valid(f value)
        | Invalid errors -> Invalid errors

    /// Monadic bind; an <c>Invalid</c> short-circuits, so errors are <em>not</em>
    /// accumulated. Use <see cref="Validation.apply"/> to gather all errors.
    let bind f =
        function
        | Valid value -> f value
        | Invalid errors -> Invalid errors

    /// Applicative apply that <em>accumulates</em> the errors of both operands.
    let apply fVal xVal =
        match fVal, xVal with
        | Valid f, Valid x -> Valid(f x)
        | Invalid e, Valid _ -> Invalid e
        | Valid _, Invalid e -> Invalid e
        | Invalid e1, Invalid e2 -> Invalid(e1 @ e2)

    /// Combines two validations with <paramref name="f"/>, accumulating errors.
    let map2 f v1 v2 = apply (map f v1) v2

    /// Combines three validations with <paramref name="f"/>, accumulating errors.
    let map3 f v1 v2 v3 = apply (apply (map f v1) v2) v3

    /// Applies <paramref name="f"/> to each element, collecting the values when
    /// all are <c>Valid</c> and <em>accumulating every error</em> otherwise.
    let traverse f list =
        List.foldBack (fun x state -> map2 (fun y ys -> y :: ys) (f x) state) list (Valid [])

    /// Turns a list of validations into a validation of the list, accumulating
    /// all errors.
    let sequence list = traverse id list

    /// The <c>Valid</c> value, or <paramref name="fallback"/> when <c>Invalid</c>.
    let defaultValue fallback =
        function
        | Valid value -> value
        | Invalid _ -> fallback

    /// The <c>Valid</c> value, or <paramref name="fallback"/> applied to the
    /// accumulated errors.
    let defaultWith fallback =
        function
        | Valid value -> value
        | Invalid errors -> fallback errors

    /// Converts to a <see cref="Result{T, E}"/> carrying the error list.
    let toResult =
        function
        | Valid value -> Ok value
        | Invalid errors -> Error errors

    /// Converts to an option, discarding any errors.
    let toOption =
        function
        | Valid value -> Some value
        | Invalid _ -> None

/// Computation-expression builder for <see cref="Validation{T}"/>. The builder
/// is monadic and therefore short-circuits; use <see cref="Validation.apply"/>
/// or <see cref="Validation.map2"/> to accumulate errors.
type ValidationBuilder() =
    member _.Return(x) = Validation.retn x
    member _.ReturnFrom(m: Validation<'T>) = m
    member _.Bind(m, f) = Validation.bind f m

[<AutoOpen>]
module ValidationExpression =
    /// Builds short-circuiting validation computations.
    let validation = ValidationBuilder()


/// The Identity monad: the simplest possible monad, wrapping a value with no
/// added effect. Mirrors the <c>Identity&lt;T&gt;</c> type in <i>Functional
/// Programming in C#</i>; chiefly useful as a building block and teaching aid.
type Identity<'T> = Identity of 'T

[<RequireQualifiedAccess>]
module Identity =
    /// Unwraps the contained value.
    let run (Identity value) = value

    /// Lifts a value into the identity context.
    let retn value = Identity value

    /// Functor map over the contained value.
    let map f (Identity value) = Identity(f value)

    /// Monadic bind over the contained value.
    let bind f (Identity value) = f value

    /// Applicative apply.
    let apply (Identity f) (Identity x) = Identity(f x)


/// A computation result that already holds either a value or the exception
/// raised while producing it - the eager sibling of the deferred
/// <see cref="Try{T}"/>. Ported from the <c>Exceptional&lt;T&gt;</c> type in
/// <i>Functional Programming in C#</i>.
[<RequireQualifiedAccess>]
type Exceptional<'T> =
    | Success of 'T
    | Exception of exn

[<RequireQualifiedAccess>]
module Exceptional =
    /// Lifts a value into a successful result.
    let retn value = Exceptional.Success value

    /// Wraps an exception as a faulted result.
    let ofException (ex: exn) : Exceptional<'T> = Exceptional.Exception ex

    /// Runs <paramref name="operation"/>, capturing any exception as a fault.
    let run operation =
        try Exceptional.Success(operation ())
        with ex -> Exceptional.Exception ex

    /// Case analysis: applies <paramref name="onException"/> to a fault or
    /// <paramref name="onSuccess"/> to a value.
    let exceptional onException onSuccess =
        function
        | Exceptional.Success value -> onSuccess value
        | Exceptional.Exception ex -> onException ex

    /// Functor map over the value; a captured exception passes through.
    let map f =
        function
        | Exceptional.Success value -> Exceptional.Success(f value)
        | Exceptional.Exception ex -> Exceptional.Exception ex

    /// Monadic bind; a captured exception short-circuits.
    let bind f =
        function
        | Exceptional.Success value -> f value
        | Exceptional.Exception ex -> Exceptional.Exception ex

    /// Applicative apply; the first exception short-circuits.
    let apply fExc xExc =
        match fExc, xExc with
        | Exceptional.Success f, Exceptional.Success x -> Exceptional.Success(f x)
        | Exceptional.Exception ex, _ -> Exceptional.Exception ex
        | _, Exceptional.Exception ex -> Exceptional.Exception ex

    /// Applicative lift of a binary function over two computations.
    let map2 f a b = apply (map f a) b

    /// Applicative lift of a ternary function over three computations.
    let map3 f a b c = apply (apply (map f a) b) c

    /// The value, or <paramref name="fallback"/> on exception.
    let defaultValue fallback =
        function
        | Exceptional.Success value -> value
        | Exceptional.Exception _ -> fallback

    /// Converts to a <see cref="Result{T, E}"/>.
    let toResult =
        function
        | Exceptional.Success value -> Ok value
        | Exceptional.Exception ex -> Error ex

    /// Converts to an option, discarding the exception.
    let toOption =
        function
        | Exceptional.Success value -> Some value
        | Exceptional.Exception _ -> None

/// Computation-expression builder for <see cref="Exceptional{T}"/>.
type ExceptionalBuilder() =
    member _.Return(x) = Exceptional.retn x
    member _.ReturnFrom(m: Exceptional<'T>) = m
    member _.Bind(m, f) = Exceptional.bind f m

[<AutoOpen>]
module ExceptionalExpression =
    /// Builds exception-capturing computations over already-evaluated results.
    let exceptional = ExceptionalBuilder()

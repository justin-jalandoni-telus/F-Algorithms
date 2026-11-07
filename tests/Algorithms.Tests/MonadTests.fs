module Algorithms.Tests.MonadTests

open Xunit
open FsUnit.Xunit
open Algorithms

[<Fact>]
let ``try captures exceptions`` () =
    match Try.run (fun () -> 1 / 0) with
    | Failure _ -> ()
    | Success _ -> failwith "expected failure"

[<Fact>]
let ``try bind short-circuits on failure`` () =
    let parse (s: string) = Try.run (fun () -> int s)

    let computation =
        tryResult {
            let! a = parse "10"
            let! b = parse "not-a-number"
            return a + b
        }

    computation |> Try.toOption |> should equal None

[<Fact>]
let ``try bind threads successes`` () =
    let parse (s: string) = Try.run (fun () -> int s)

    let computation =
        tryResult {
            let! a = parse "10"
            let! b = parse "32"
            return a + b
        }

    computation |> Try.toOption |> should equal (Some 42)

[<Fact>]
let ``try applicative combines successes`` () =
    let add a b = a + b
    Try.apply (Try.map add (Success 2)) (Success 3) |> should equal (Success 5)

[<Fact>]
let ``reader threads the environment`` () =
    let greeting =
        reader {
            let! name = Reader.asks (fun (env: string) -> env.ToUpper())
            return $"Hello, {name}"
        }

    Reader.run "world" greeting |> should equal "Hello, WORLD"

[<Fact>]
let ``state threads updates`` () =
    let counter =
        state {
            let! current = State.get
            do! State.put (current + 1)
            return current
        }

    State.run 41 counter |> should equal (41, 42)

[<Fact>]
let ``option applicative combines values`` () =
    let add a b = a + b
    OptionM.apply (OptionM.map add (Some 2)) (Some 3) |> should equal (Some 5)

[<Fact>]
let ``result applicative returns the first error`` () =
    let add a b = a + b

    ResultM.apply (ResultM.map add (Ok 2)) (Error "bad")
    |> should equal (Error "bad": Result<int, string>)

[<Fact>]
let ``either bind threads right values`` () =
    let halve x =
        if x % 2 = 0 then Right(x / 2) else Left "odd"

    let computation =
        either {
            let! a = halve 20
            let! b = halve a
            return a + b
        }

    computation |> should equal (Right 15: Either<string, int>)

[<Fact>]
let ``either bind short-circuits on the first left`` () =
    let halve x =
        if x % 2 = 0 then Right(x / 2) else Left "odd"

    let computation =
        either {
            let! a = halve 20
            let! b = halve 5
            return a + b
        }

    computation |> should equal (Left "odd": Either<string, int>)

[<Fact>]
let ``either apply short-circuits to the first left`` () =
    let add a b = a + b

    Either.apply (Either.map add (Left "bad")) (Right 3)
    |> should equal (Left "bad": Either<string, int>)

[<Fact>]
let ``either converts to and from result`` () =
    (Right 42: Either<string, int>) |> Either.toResult |> should equal (Ok 42: Result<int, string>)
    (Error "bad": Result<int, string>) |> Either.ofResult |> should equal (Left "bad": Either<string, int>)

[<Fact>]
let ``validation apply accumulates all errors`` () =
    let err message = ValidationError.create message
    let add a b = a + b

    let combined =
        Validation.apply (Validation.map add (Invalid [ err "first" ])) (Invalid [ err "second" ])

    combined
    |> should equal (Invalid [ err "first"; err "second" ]: Validation<int>)

[<Fact>]
let ``validation map2 accumulates errors from both operands`` () =
    let add a b = a + b

    Validation.map2 add (Validation.failWith "a") (Validation.failWith "b")
    |> Validation.toResult
    |> should equal (Error [ ValidationError.create "a"; ValidationError.create "b" ]: Result<int, ValidationError list>)

[<Fact>]
let ``validation map2 combines valid values`` () =
    let add a b = a + b

    Validation.map2 add (Valid 2) (Valid 3) |> should equal (Valid 5)

[<Fact>]
let ``validation bind short-circuits without accumulating`` () =
    let computation =
        validation {
            let! a = Valid 10
            let! b = Validation.failWith "boom"
            return a + b
        }

    computation |> should equal (Validation.failWith "boom": Validation<int>)

[<Fact>]
let ``identity bind threads the wrapped value`` () =
    Identity 20
    |> Identity.bind (fun x -> Identity(x + 1))
    |> Identity.map (fun x -> x * 2)
    |> Identity.run
    |> should equal 42

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

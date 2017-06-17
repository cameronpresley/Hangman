module HangmanModule
open System

type GameState = Ongoing | Won | Lost

let private doesElementExist word letter = word |> Seq.tryFind(fun x -> x = letter)

let createDisplayWord word guesses =
    let mapGuessToChar guesses letter =
        match doesElementExist guesses letter with
        | None -> '-'
        | Some x -> x

    word |> Seq.map(fun x -> mapGuessToChar guesses x) |> String.Concat

let calculateIncorrectGuesses word guesses =
    guesses
        |> Seq.map(fun x -> doesElementExist word x)
        |> Seq.filter(Option.isNone)
        |> Seq.length

let determineState word guesses =
    match guesses |> calculateIncorrectGuesses word with
    | lost when lost >= 6 -> Lost
    | _ ->
        match guesses |> createDisplayWord word with
        | same when same = word -> Won
        | _ -> Ongoing
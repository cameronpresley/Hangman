module Output

open System
open HangmanModule

let createPicture incorrectGuesses =
    match incorrectGuesses with
    | 0 -> "Good so far..."
    | 1 -> "Head"
    | 2 -> "Head and left arm"
    | 3 -> "Head and arms"
    | 4 -> "Head, arms, and torso"
    | 5 -> "Head, arms, torso, left leg"
    | 6 -> "Head, arms, torso, legs"
    | _ -> "No Image to display"

let createDisplay word guesses =
    guesses |> calculateIncorrectGuesses word |> createPicture |> printfn "%s"
    printfn ""
    printfn ""
    guesses |> createDisplayWord word |> printfn "Word: %s"
    guesses |> printfn "Guessed Letters: %s"

let rec getUserGuess guesses =
    match Console.ReadLine().ToLower() with
    | incorrect when incorrect.Length <> 1 -> getUserGuess guesses
    | guess -> 
        let guess = guess |> Seq.head
        match guesses |> Seq.tryFind(fun x -> guess = x) with
        | None -> String.Concat(guesses, guess.ToString()) 
                    |> Seq.sort 
                    |> String.Concat
        | _ -> getUserGuess guesses
    
let rec takeTurn word guesses =
    Console.Clear()
    createDisplay word guesses
    match guesses |> determineState word with
    | Won -> printfn "You Won!"
    | Lost -> printfn "You Lost, the word was %s" word
    | _ -> 
        let guesses = guesses |> getUserGuess
        takeTurn word guesses
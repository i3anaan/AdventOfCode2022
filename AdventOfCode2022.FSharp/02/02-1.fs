type Move = Rock | Paper | Scissor

let convertToMove (i: string) = 
    match i with
        | "A" | "X" -> Rock
        | "B" | "Y" -> Paper
        | "C" | "Z" -> Scissor

let calculateMoveScore (move: Move): int =
    match move with
        | Rock -> 1
        | Paper -> 2
        | Scissor -> 3

let calculateWin (myMove: Move) (oppMove: Move) = 
    match (myMove, oppMove) with
        | (Paper, Rock) -> true
        | (Scissor, Paper) -> true
        | (Rock, Scissor) -> true
        | (_, _) -> false
        
let calculateWinScore (myMove: Move) (oppMove: Move) =
    match (calculateWin myMove oppMove, calculateWin oppMove myMove ) with
        | (true, false) -> 6
        | (false, true) -> 0
        | _ -> 3

let calculateScore (tuple: string * string): int =
    let moves = (fst tuple |> convertToMove, snd tuple |> convertToMove)
    calculateMoveScore (snd moves) + calculateWinScore (snd moves ) (fst moves)

let readLines filePath = System.IO.File.ReadLines(filePath);;
let lines = readLines "AdventOfCode2022.FSharp/02/input.txt"
let tuples = Seq.map (fun (l: string) -> l.Split ' ') lines |> Seq.map (fun (l: string[]) -> (l[0], l[1]))

let scores = tuples |> (Seq.map calculateScore)

let rec sum i = 
    match i with
        | [] -> 0
        | x::xs -> x + (sum xs)

printfn "Answer: %d" (Seq.toList scores |> sum)
let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines "AdventOfCode2022.FSharp/03/input.txt"

let charToValue (c: char) : int =
    match (int c - int 'a' >= 0) with
    | true -> int c - int 'a' + 1
    | false -> int c - int 'A' + 1 + 26

let linesToSets: (seq<string> -> list<Set<char>>) =
    Seq.toList >> (List.map Set.ofSeq)

let rec linesToValue (lines: list<Set<char>>) =
    match Seq.toList lines |> (List.map Set.ofSeq) with
    | [] -> 0
    | x :: y :: z :: tail ->
        let badge =
            Set.toArray (Set.intersect x y |> Set.intersect z)
            |> Array.head
            |> charToValue

        badge + (linesToValue tail)

let results = linesToSets lines |> linesToValue
// Answer: 2581

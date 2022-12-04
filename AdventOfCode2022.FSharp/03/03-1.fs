let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines "AdventOfCode2022.FSharp/03/input.txt"

let lineToSets (line: string) : Set<char> * Set<char> =
    match Seq.toList line with
    | [] -> (Set.empty<char>, Set.empty<char>)
    | list ->
        let halves = (2, list) ||> List.splitInto
        let folder = List.fold (fun (s: Set<char>) (c: char) -> (s.Add c)) Set.empty<char>
        let sets = List.map folder halves
        (sets[0], sets[1])

let charToValue (c: char) : int =
    match (int c - int 'a' >= 0) with
    | true -> int c - int 'a' + 1
    | false -> int c - int 'A' + 1 + 26

let lineToValue (line: string) : int =
    lineToSets line
    ||> Set.intersect
    |> (Set.toArray >> Array.head >> charToValue)

let results = Seq.map lineToValue lines

Seq.sum results
// Answer: 7850

module _06_02

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines "C:\Users\Eigenaar\Projects\AdventOfCode2022\AdventOfCode2022.FSharp.Console\06\input.txt"
let line = (Seq.toList lines)[0]

let rec findUnique14 (index: int) (buffer: list<char>) (remaining: list<char>): int =
    printfn "index: %d buffer: %A remaining: %O" index buffer remaining
    match List.length buffer with
    | x when x < 14 -> findUnique14 (index + 1) ([List.head remaining] @ buffer) (List.tail remaining)
    | x ->  match (List.distinct buffer |> List.length) with
            | x when x < 14 -> findUnique14 (index + 1) ([List.head remaining] @ (List.splitAt 13 buffer |> fst)) (List.tail remaining)
            | x when x >= 14 -> index
            | _ -> failwith "?"

Seq.toList line
|> findUnique14 0 [] 


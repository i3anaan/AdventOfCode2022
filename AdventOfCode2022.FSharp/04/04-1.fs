let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines "AdventOfCode2022.FSharp/04/input.txt"

let listToTuple (p: 'a []) = (p[0], p[1])

let processLine (line: string) =
    line.Split ','
    |> Array.map (fun s -> s.Split '-')
    |> (Array.map listToTuple)
    >> listToTuple
    |> printfn "%O"

    3

lines
|> Seq.take 3
|> Seq.map processLine
|> Seq.sum

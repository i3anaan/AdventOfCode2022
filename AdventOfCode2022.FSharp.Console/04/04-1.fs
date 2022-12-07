module _04_1

let calculate =
    let readLines filePath = System.IO.File.ReadLines(filePath)
    let lines = readLines "C:\Users\Eigenaar\Projects\AdventOfCode2022\AdventOfCode2022.FSharp.Console\04\input.txt"

    let listToTuple (p: 'a []) = (p[0], p[1])

    let processLine (line: string) =
        line.Split ','
        |> Array.map (fun s -> s.Split '-')
        |> Array.map listToTuple
        |> Array.map (fun (s, e) -> (int s, int e))
        |> Array.map (fun (s, e) -> Set.ofList [s..e])
        |> listToTuple
        ||> (fun s1 s2 -> Set.isSubset s1 s2 || Set.isSubset s2 s1)
        |> (fun result -> if result then 1 else 0)

    lines
    |> Seq.map processLine
    |> Seq.sum

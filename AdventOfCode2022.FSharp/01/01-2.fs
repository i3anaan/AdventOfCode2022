// https://dungpa.github.io/fsharp-cheatsheet/
let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "AdventOfCode2022.FSharp/01/input.txt"



let safeParse (str: string) = 
    match System.Int32.TryParse str with
        | true, value -> Some value
        | _ -> None

let parsed: seq<option<int>> = Seq.map (fun x -> safeParse x) lines 

let rec summed (input: list<option<int>>) (cum: int) = 
    match input with
        | [] -> 
            printfn "Empty"
            // failwith "crash"
            [cum]
        | x :: xs -> 
            match x with 
                | Some x -> 
                    printfn "Some %d %O" x xs 
                    summed xs (cum + x)
                | None -> 
                    printfn "None %O" xs
                    [cum] @ (summed xs 0)
//                | Some x -> (x + (fst summed xs), (snd summed xs))
//                | None -> (0, snd summed xs)


let result: list<int> = summed (Seq.toList parsed) 0

let rec maxList (input: list<int>) (currentMax: int): int =
    match input with
    | [] -> currentMax
    | x::xs -> 
        match x > currentMax with
            | true -> maxList xs x
            | false -> maxList xs currentMax

(maxList result 0) |> printfn "Max: %d" 

let sorted = List.sort result |> List.rev

let rec sumFirstX (input: list<int>) (amount: int) =
    match (amount, input) with
        | (0, _) -> 0
        | (_, []) -> 0
        | (_, x::xs) -> x + (sumFirstX xs (amount - 1))

sumFirstX sorted 3 |> printfn "value: %d" 

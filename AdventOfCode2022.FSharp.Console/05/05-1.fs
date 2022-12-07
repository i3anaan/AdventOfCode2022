module _05_1

(*
[P]     [L]         [T]            
[L]     [M] [G]     [G]     [S]    
[M]     [Q] [W]     [H] [R] [G]    
[N]     [F] [M]     [D] [V] [R] [N]
[W]     [G] [Q] [P] [J] [F] [M] [C]
[V] [H] [B] [F] [H] [M] [B] [H] [B]
[B] [Q] [D] [T] [T] [B] [N] [L] [D]
[H] [M] [N] [Z] [M] [C] [M] [P] [P]
 1   2   3   4   5   6   7   8   9 

move 8 from 3 to 2
move 1 from 9 to 5
move 5 from 4 to 7
move 6 from 1 to 4
move 8 from 6 to 8
move 8 from 4 to 5
move 4 from 9 to 5
move 4 from 7 to 9
move 7 from 7 to 2
*)

type Crate = char
type Stack = list<Crate>
type State = array<Stack>

let calculate =
    let readLines filePath = System.IO.File.ReadLines(filePath)
    let lines = readLines "C:\Users\Eigenaar\Projects\AdventOfCode2022\AdventOfCode2022.FSharp.Console\05\input.txt"


    let stacks: State = [|
        ['P'; 'L'; 'M'; 'N'; 'W'; 'V'; 'B'; 'H' ];
        ['H'; 'Q'; 'M']
    |]

    let processLine (state: State) (line: string): State =
        
    let rec from (index: int) (state: State): State * Crate = 
        match (index - 1) with
            | 0 ->  let stack = Array.head state
                    let crate = stack |> List.head
                    let newStack: Stack = List.tail stack
                    let newState = Array.append [|newStack|] (Array.tail state)
                    (newState, crate)
            | x ->  let (remainder, crate) = Array.tail state |> from x
                    (Array.append [|(Array.head state)|] remainder, crate)

    let rec towards (index: int) (state: State) (crate: char): State = 
        match (index - 1) with
            | 0 ->  let stack = Array.head state
                    let newStack: Stack = List.append [crate] stack
                    let newState = Array.append [|newStack|] (Array.tail state)
                    newState
            | x ->  let (remainder, crate) =  towards x (Array.tail state) crate
                    Array.append [|(Array.head state)|] remainder

    stacks
    from 1 stacks
    towards 2 (from 1 stacks)

    let rec move (state: State) (count: int) (fromSelector: State -> Stack) (toSelector: State -> Stack) =
        let x::xs = fromSelector state
        (toSelector state)


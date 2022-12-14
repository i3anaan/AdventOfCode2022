module _05_1

type Crate = char
type Stack = list<Crate>
type State = array<Stack>

let calculate =

    let stacks: State = [|
        ['P'; 'L'; 'M'; 'N'; 'W'; 'V'; 'B'; 'H' ];
        ['H'; 'Q'; 'M'];
        ['L'; 'M'; 'Q'; 'F'; 'G'; 'B'; 'D'; 'N'];
        ['G'; 'W'; 'M'; 'Q'; 'F'; 'T'; 'Z'];
        ['P'; 'H'; 'T'; 'M'];
        ['T'; 'G'; 'H'; 'D'; 'J'; 'M'; 'B'; 'C'];
        ['R'; 'V'; 'F'; 'B'; 'N'; 'M'];
        ['S'; 'G'; 'R'; 'M'; 'H'; 'L'; 'P'];
        ['N'; 'C'; 'B'; 'D'; 'P'];
    |]

    let rec from (index: int) (amount: int) (state: State): State * list<Crate> = 
        match (index - 1) with
            | 0 ->  let stack = Array.head state
                    printfn "Stack %O amount %d" stack amount
                    let split = stack |> List.splitAt amount
                    printfn "%O" split
                    let crates = fst split
                    let newStack: Stack = snd split
                    let newState = Array.append [|newStack|] (Array.tail state)
                    (newState, crates)
            | x ->  let (remainder, crates) = Array.tail state |> from x amount
                    (Array.append [|(Array.head state)|] remainder, crates)

    let rec towards (index: int) (state: State) (crates: list<Crate>): State = 
        match (index - 1) with
            | 0 ->  let stack = Array.head state
                    let newStack: Stack = List.append crates stack
                    let newState = Array.append [|newStack|] (Array.tail state)
                    newState
            | x ->  let remainder =  towards x (Array.tail state) crates
                    Array.append [|(Array.head state)|] remainder


    let rec move (count: int) (fromSelector: int -> State -> State * list<Crate>) (toSelector: State -> list<Crate> -> State) (state: State): State =
        fromSelector count state ||> toSelector

    // Answer: GMPMLWNMG
    move 8 (from 3) (towards 2) stacks
    |> move 1 (from 9) (towards 5)
    |> move 5 (from 4) (towards 7)
    |> move 6 (from 1) (towards 4)
    |> move 8 (from 6) (towards 8)
    |> move 8 (from 4) (towards 5)
    |> move 4 (from 9) (towards 5)
    |> move 4 (from 7) (towards 9)
    |> move 7 (from 7) (towards 2)
    |> move 4 (from 5) (towards 2)
    |> move 11 (from 8) (towards 3)
    |> move 3 (from 9) (towards 7)
    |> move 11 (from 2) (towards 8)
    |> move 13 (from 8) (towards 4)
    |> move 11 (from 5) (towards 6)
    |> move 8 (from 2) (towards 4)
    |> move 1 (from 5) (towards 4)
    |> move 1 (from 3) (towards 2)
    |> move 2 (from 2) (towards 1)
    |> move 2 (from 8) (towards 5)
    |> move 3 (from 7) (towards 5)
    |> move 1 (from 4) (towards 7)
    |> move 9 (from 6) (towards 7)
    |> move 1 (from 6) (towards 5)
    |> move 1 (from 1) (towards 4)
    |> move 3 (from 1) (towards 9)
    |> move 15 (from 4) (towards 3)
    |> move 2 (from 4) (towards 1)
    |> move 1 (from 1) (towards 9)
    |> move 3 (from 4) (towards 5)
    |> move 1 (from 4) (towards 1)
    |> move 1 (from 7) (towards 2)
    |> move 1 (from 6) (towards 3)
    |> move 5 (from 7) (towards 1)
    |> move 19 (from 3) (towards 9)
    |> move 7 (from 1) (towards 2)
    |> move 24 (from 9) (towards 7)
    |> move 23 (from 7) (towards 1)
    |> move 1 (from 4) (towards 6)
    |> move 3 (from 7) (towards 3)
    |> move 1 (from 6) (towards 1)
    |> move 6 (from 2) (towards 1)
    |> move 21 (from 1) (towards 9)
    |> move 5 (from 3) (towards 8)
    |> move 2 (from 2) (towards 5)
    |> move 10 (from 9) (towards 5)
    |> move 1 (from 2) (towards 1)
    |> move 5 (from 1) (towards 3)
    |> move 6 (from 3) (towards 4)
    |> move 1 (from 2) (towards 8)
    |> move 3 (from 5) (towards 2)
    |> move 4 (from 9) (towards 3)
    |> move 13 (from 5) (towards 9)
    |> move 2 (from 7) (towards 2)
    |> move 3 (from 4) (towards 7)
    |> move 1 (from 7) (towards 8)
    |> move 5 (from 1) (towards 3)
    |> move 1 (from 7) (towards 5)
    |> move 1 (from 8) (towards 1)
    |> move 2 (from 2) (towards 7)
    |> move 19 (from 9) (towards 2)
    |> move 5 (from 2) (towards 3)
    |> move 7 (from 5) (towards 9)
    |> move 1 (from 1) (towards 9)
    |> move 5 (from 9) (towards 2)
    |> move 4 (from 9) (towards 3)
    |> move 20 (from 3) (towards 9)
    |> move 1 (from 3) (towards 9)
    |> move 3 (from 7) (towards 3)
    |> move 16 (from 2) (towards 3)
    |> move 12 (from 3) (towards 4)
    |> move 2 (from 2) (towards 5)
    |> move 1 (from 2) (towards 4)
    |> move 2 (from 4) (towards 1)
    |> move 4 (from 8) (towards 1)
    |> move 15 (from 9) (towards 3)
    |> move 2 (from 5) (towards 3)
    |> move 3 (from 2) (towards 8)
    |> move 5 (from 8) (towards 5)
    |> move 7 (from 3) (towards 4)
    |> move 2 (from 9) (towards 6)
    |> move 15 (from 3) (towards 1)
    |> move 3 (from 1) (towards 8)
    |> move 3 (from 9) (towards 5)
    |> move 9 (from 4) (towards 1)
    |> move 3 (from 3) (towards 5)
    |> move 2 (from 6) (towards 5)
    |> move 9 (from 1) (towards 3)
    |> move 1 (from 9) (towards 4)
    |> move 1 (from 5) (towards 2)
    |> move 3 (from 8) (towards 5)
    |> move 10 (from 1) (towards 6)
    |> move 12 (from 4) (towards 8)
    |> move 1 (from 2) (towards 7)
    |> move 2 (from 5) (towards 6)
    |> move 1 (from 1) (towards 4)
    |> move 7 (from 3) (towards 6)
    |> move 1 (from 7) (towards 2)
    |> move 2 (from 4) (towards 9)
    |> move 3 (from 1) (towards 7)
    |> move 1 (from 9) (towards 8)
    |> move 1 (from 2) (towards 3)
    |> move 3 (from 1) (towards 7)
    |> move 5 (from 8) (towards 2)
    |> move 5 (from 7) (towards 1)
    |> move 9 (from 6) (towards 8)
    |> move 6 (from 6) (towards 9)
    |> move 8 (from 8) (towards 6)
    |> move 1 (from 7) (towards 4)
    |> move 5 (from 2) (towards 4)
    |> move 7 (from 5) (towards 1)
    |> move 5 (from 8) (towards 9)
    |> move 11 (from 6) (towards 7)
    |> move 9 (from 9) (towards 1)
    |> move 2 (from 7) (towards 5)
    |> move 1 (from 9) (towards 5)
    |> move 1 (from 3) (towards 6)
    |> move 3 (from 4) (towards 6)
    |> move 1 (from 8) (towards 2)
    |> move 2 (from 3) (towards 6)
    |> move 6 (from 5) (towards 2)
    |> move 3 (from 5) (towards 9)
    |> move 3 (from 2) (towards 1)
    |> move 1 (from 4) (towards 3)
    |> move 3 (from 2) (towards 7)
    |> move 1 (from 8) (towards 9)
    |> move 1 (from 2) (towards 8)
    |> move 8 (from 7) (towards 5)
    |> move 1 (from 7) (towards 8)
    |> move 3 (from 5) (towards 6)
    |> move 5 (from 5) (towards 2)
    |> move 1 (from 4) (towards 1)
    |> move 1 (from 3) (towards 2)
    |> move 4 (from 1) (towards 5)
    |> move 4 (from 2) (towards 6)
    |> move 6 (from 1) (towards 2)
    |> move 5 (from 9) (towards 3)
    |> move 2 (from 5) (towards 3)
    |> move 3 (from 3) (towards 6)
    |> move 10 (from 6) (towards 4)
    |> move 4 (from 8) (towards 5)
    |> move 5 (from 5) (towards 1)
    |> move 21 (from 1) (towards 7)
    |> move 3 (from 2) (towards 9)
    |> move 1 (from 5) (towards 2)
    |> move 4 (from 2) (towards 9)
    |> move 8 (from 4) (towards 8)
    |> move 1 (from 2) (towards 1)
    |> move 7 (from 8) (towards 2)
    |> move 2 (from 6) (towards 1)
    |> move 2 (from 1) (towards 5)
    |> move 1 (from 1) (towards 5)
    |> move 4 (from 3) (towards 7)
    |> move 1 (from 9) (towards 3)
    |> move 4 (from 6) (towards 3)
    |> move 1 (from 3) (towards 8)
    |> move 1 (from 3) (towards 4)
    |> move 2 (from 2) (towards 6)
    |> move 2 (from 9) (towards 7)
    |> move 14 (from 7) (towards 8)
    |> move 10 (from 8) (towards 7)
    |> move 3 (from 4) (towards 6)
    |> move 5 (from 2) (towards 3)
    |> move 3 (from 9) (towards 8)
    |> move 3 (from 3) (towards 4)
    |> move 1 (from 2) (towards 4)
    |> move 1 (from 9) (towards 4)
    |> move 1 (from 9) (towards 5)
    |> move 1 (from 5) (towards 2)
    |> move 3 (from 5) (towards 7)
    |> move 1 (from 4) (towards 6)
    |> move 5 (from 3) (towards 8)
    |> move 1 (from 6) (towards 8)
    |> move 5 (from 7) (towards 6)
    |> move 14 (from 8) (towards 5)
    |> move 2 (from 6) (towards 7)
    |> move 18 (from 7) (towards 2)
    |> move 3 (from 6) (towards 1)
    |> move 5 (from 5) (towards 4)
    |> move 5 (from 6) (towards 2)
    |> move 7 (from 2) (towards 1)
    |> move 1 (from 8) (towards 4)
    |> move 1 (from 5) (towards 1)
    |> move 8 (from 1) (towards 9)
    |> move 10 (from 4) (towards 3)
    |> move 8 (from 5) (towards 3)
    |> move 1 (from 4) (towards 3)
    |> move 2 (from 1) (towards 5)
    |> move 1 (from 5) (towards 3)
    |> move 5 (from 3) (towards 1)
    |> move 1 (from 1) (towards 3)
    |> move 5 (from 1) (towards 6)
    |> move 13 (from 3) (towards 1)
    |> move 3 (from 9) (towards 4)
    |> move 2 (from 9) (towards 6)
    |> move 5 (from 6) (towards 5)
    |> move 6 (from 5) (towards 1)
    |> move 7 (from 7) (towards 9)
    |> move 7 (from 9) (towards 6)
    |> move 1 (from 9) (towards 3)
    |> move 1 (from 7) (towards 9)
    |> move 3 (from 9) (towards 1)
    |> move 12 (from 2) (towards 7)
    |> move 7 (from 6) (towards 2)
    |> move 22 (from 1) (towards 7)
    |> move 1 (from 6) (towards 5)
    |> move 4 (from 7) (towards 6)
    |> move 1 (from 5) (towards 6)
    |> move 2 (from 4) (towards 1)
    |> move 1 (from 4) (towards 1)
    |> move 23 (from 7) (towards 9)
    |> move 4 (from 6) (towards 2)
    |> move 4 (from 7) (towards 3)
    |> move 1 (from 1) (towards 9)
    |> move 6 (from 2) (towards 1)
    |> move 1 (from 7) (towards 2)
    |> move 7 (from 2) (towards 8)
    |> move 2 (from 3) (towards 8)
    |> move 3 (from 1) (towards 9)
    |> move 1 (from 2) (towards 8)
    |> move 5 (from 8) (towards 3)
    |> move 3 (from 2) (towards 1)
    |> move 2 (from 7) (towards 8)
    |> move 10 (from 9) (towards 8)
    |> move 4 (from 1) (towards 3)
    |> move 14 (from 3) (towards 4)
    |> move 7 (from 4) (towards 5)
    |> move 1 (from 6) (towards 9)
    |> move 5 (from 5) (towards 8)
    |> move 1 (from 6) (towards 4)
    |> move 6 (from 9) (towards 4)
    |> move 3 (from 8) (towards 4)
    |> move 1 (from 5) (towards 1)
    |> move 3 (from 4) (towards 3)
    |> move 9 (from 4) (towards 3)
    |> move 5 (from 3) (towards 6)
    |> move 5 (from 1) (towards 5)
    |> move 4 (from 6) (towards 2)
    |> move 8 (from 9) (towards 2)
    |> move 2 (from 6) (towards 5)
    |> move 3 (from 4) (towards 7)
    |> move 2 (from 2) (towards 7)
    |> move 2 (from 5) (towards 4)
    |> move 3 (from 5) (towards 9)
    |> move 3 (from 4) (towards 2)
    |> move 10 (from 2) (towards 5)
    |> move 1 (from 9) (towards 8)
    |> move 2 (from 2) (towards 9)
    |> move 3 (from 7) (towards 2)
    |> move 1 (from 2) (towards 9)
    |> move 13 (from 5) (towards 1)
    |> move 2 (from 2) (towards 7)
    |> move 8 (from 9) (towards 2)
    |> move 1 (from 4) (towards 6)
    |> move 1 (from 9) (towards 5)
    |> move 14 (from 8) (towards 4)
    |> move 7 (from 4) (towards 5)
    |> move 4 (from 7) (towards 5)
    |> move 2 (from 3) (towards 8)
    |> move 4 (from 1) (towards 5)
    |> move 2 (from 5) (towards 4)
    |> move 6 (from 5) (towards 6)
    |> move 7 (from 2) (towards 5)
    |> move 1 (from 2) (towards 6)
    |> move 1 (from 5) (towards 2)
    |> move 2 (from 2) (towards 8)
    |> move 2 (from 1) (towards 3)
    |> move 8 (from 4) (towards 7)
    |> move 1 (from 4) (towards 3)
    |> move 6 (from 1) (towards 6)
    |> move 7 (from 3) (towards 9)
    |> move 3 (from 7) (towards 1)
    |> move 2 (from 8) (towards 7)
    |> move 7 (from 6) (towards 9)
    |> move 2 (from 3) (towards 6)
    |> move 6 (from 8) (towards 3)
    |> move 9 (from 5) (towards 3)
    |> move 2 (from 7) (towards 8)
    |> move 2 (from 6) (towards 4)
    |> move 7 (from 6) (towards 9)
    |> move 5 (from 3) (towards 8)
    |> move 10 (from 9) (towards 1)
    |> move 11 (from 1) (towards 8)
    |> move 1 (from 3) (towards 2)
    |> move 4 (from 5) (towards 6)
    |> move 2 (from 6) (towards 2)
    |> move 2 (from 7) (towards 9)
    |> move 3 (from 1) (towards 7)
    |> move 6 (from 3) (towards 9)
    |> move 2 (from 7) (towards 2)
    |> move 2 (from 6) (towards 9)
    |> move 1 (from 5) (towards 9)
    |> move 11 (from 9) (towards 8)
    |> move 1 (from 4) (towards 5)
    |> move 6 (from 9) (towards 8)
    |> move 31 (from 8) (towards 9)
    |> move 1 (from 3) (towards 6)
    |> move 1 (from 7) (towards 1)
    |> move 1 (from 4) (towards 3)
    |> move 1 (from 5) (towards 2)
    |> move 1 (from 1) (towards 8)
    |> move 1 (from 8) (towards 9)
    |> move 1 (from 7) (towards 3)
    |> move 11 (from 9) (towards 6)
    |> move 2 (from 3) (towards 1)
    |> move 2 (from 3) (towards 5)
    |> move 1 (from 5) (towards 4)
    |> move 1 (from 4) (towards 1)
    |> move 6 (from 8) (towards 3)
    |> move 1 (from 1) (towards 4)
    |> move 1 (from 4) (towards 6)
    |> move 2 (from 3) (towards 6)
    |> move 17 (from 9) (towards 2)
    |> move 23 (from 2) (towards 9)
    |> move 14 (from 9) (towards 4)
    |> move 1 (from 1) (towards 7)
    |> move 1 (from 5) (towards 6)
    |> move 8 (from 6) (towards 2)
    |> move 1 (from 3) (towards 2)
    |> move 4 (from 9) (towards 8)
    |> move 5 (from 4) (towards 7)
    |> move 3 (from 7) (towards 2)
    |> move 1 (from 1) (towards 2)
    |> move 2 (from 9) (towards 4)
    |> move 3 (from 6) (towards 9)
    |> move 8 (from 4) (towards 9)
    |> move 2 (from 4) (towards 2)
    |> move 4 (from 7) (towards 2)
    |> move 1 (from 7) (towards 9)
    |> move 4 (from 6) (towards 2)
    |> move 16 (from 2) (towards 1)
    |> move 2 (from 3) (towards 2)
    |> move 18 (from 9) (towards 8)
    |> move 1 (from 4) (towards 2)
    |> move 1 (from 6) (towards 8)
    |> move 1 (from 3) (towards 9)
    |> move 3 (from 9) (towards 5)
    |> move 4 (from 9) (towards 8)
    |> move 6 (from 2) (towards 8)
    |> move 1 (from 5) (towards 1)
    |> move 4 (from 2) (towards 8)
    |> move 1 (from 5) (towards 1)
    |> move 17 (from 1) (towards 4)
    |> move 1 (from 5) (towards 8)
    |> move 10 (from 4) (towards 3)
    |> move 10 (from 3) (towards 1)
    |> move 4 (from 4) (towards 9)
    |> move 1 (from 4) (towards 6)
    |> move 1 (from 4) (towards 8)
    |> move 38 (from 8) (towards 1)
    |> move 27 (from 1) (towards 5)
    |> move 1 (from 8) (towards 2)
    |> move 1 (from 6) (towards 3)
    |> move 1 (from 4) (towards 8)
    |> move 1 (from 8) (towards 4)
    |> move 14 (from 1) (towards 9)
    |> move 1 (from 3) (towards 1)
    |> move 1 (from 5) (towards 1)
    |> move 1 (from 2) (towards 5)
    |> move 2 (from 5) (towards 4)
    |> move 17 (from 5) (towards 8)
    |> move 3 (from 4) (towards 9)
    |> move 2 (from 9) (towards 1)
    |> move 3 (from 5) (towards 7)
    |> move 3 (from 7) (towards 4)
    |> move 2 (from 4) (towards 7)
    |> move 12 (from 1) (towards 4)
    |> move 1 (from 7) (towards 4)
    |> move 1 (from 7) (towards 6)
    |> move 1 (from 6) (towards 9)
    |> move 11 (from 4) (towards 3)
    |> move 1 (from 5) (towards 3)
    |> move 11 (from 3) (towards 9)
    |> move 1 (from 3) (towards 2)
    |> move 3 (from 5) (towards 4)
    |> move 1 (from 2) (towards 4)
    |> move 1 (from 5) (towards 8)
    |> move 13 (from 9) (towards 3)
    |> move 16 (from 9) (towards 1)
    |> move 4 (from 8) (towards 9)
    |> move 2 (from 1) (towards 4)
    |> move 1 (from 9) (towards 1)
    |> move 1 (from 9) (towards 7)
    |> move 1 (from 7) (towards 2)
    |> move 6 (from 8) (towards 3)
    |> move 8 (from 4) (towards 2)
    |> move 4 (from 9) (towards 6)
    |> move 3 (from 2) (towards 3)
    |> move 3 (from 6) (towards 1)
    |> move 3 (from 8) (towards 6)
    |> move 1 (from 6) (towards 8)
    |> move 3 (from 6) (towards 4)
    |> move 11 (from 3) (towards 5)
    |> move 4 (from 8) (towards 2)
    |> move 6 (from 3) (towards 5)
    |> move 3 (from 5) (towards 1)
    |> move 2 (from 8) (towards 3)
    |> move 14 (from 5) (towards 3)
    |> move 4 (from 3) (towards 4)
    |> move 6 (from 3) (towards 5)
    |> move 3 (from 2) (towards 9)
    |> move 4 (from 1) (towards 8)
    |> move 3 (from 9) (towards 6)
    |> move 2 (from 6) (towards 9)
    |> move 6 (from 4) (towards 3)
    |> move 15 (from 1) (towards 4)
    |> move 1 (from 6) (towards 7)
    |> move 5 (from 5) (towards 1)
    |> move 11 (from 3) (towards 1)
    |> move 2 (from 9) (towards 7)
    |> move 1 (from 5) (towards 6)
    |> move 2 (from 1) (towards 3)
    |> move 7 (from 2) (towards 6)
    |> move 4 (from 8) (towards 1)
    |> move 8 (from 4) (towards 2)
    |> move 3 (from 6) (towards 4)
    |> move 5 (from 1) (towards 4)
    |> move 17 (from 4) (towards 8)
    |> move 3 (from 3) (towards 7)
    |> move 4 (from 3) (towards 4)
    |> move 4 (from 4) (towards 2)
    |> move 9 (from 8) (towards 7)
    |> move 1 (from 3) (towards 8)
    |> move 10 (from 2) (towards 4)
    |> move 1 (from 6) (towards 2)
    |> move 2 (from 8) (towards 4)
    |> move 2 (from 6) (towards 9)
    |> move 2 (from 6) (towards 2)
    |> move 1 (from 2) (towards 3)
    |> move 3 (from 1) (towards 4)
    |> move 1 (from 3) (towards 2)
    |> move 1 (from 9) (towards 3)
    |> move 1 (from 9) (towards 7)
    |> move 4 (from 8) (towards 4)
    |> move 10 (from 4) (towards 8)
    |> move 5 (from 4) (towards 3)
    |> move 1 (from 2) (towards 8)
    |> move 5 (from 3) (towards 7)
    |> move 3 (from 7) (towards 8)
    |> move 3 (from 4) (towards 3)
    |> move 8 (from 7) (towards 2)
    |> move 8 (from 7) (towards 8)
    |> move 1 (from 3) (towards 2)
    |> move 3 (from 2) (towards 8)
    |> move 9 (from 2) (towards 5)
    |> move 12 (from 1) (towards 7)
    |> move 21 (from 8) (towards 3)
    |> move 5 (from 8) (towards 6)
    |> move 8 (from 7) (towards 5)
    |> move 6 (from 7) (towards 4)
    |> move 12 (from 5) (towards 7)
    |> move 1 (from 8) (towards 5)
    |> move 2 (from 4) (towards 2)
    |> move 1 (from 7) (towards 6)
    |> move 14 (from 3) (towards 8)
    |> move 5 (from 6) (towards 2)
    |> move 7 (from 2) (towards 6)
    |> move 6 (from 8) (towards 4)
    |> move 11 (from 7) (towards 4)
    |> move 8 (from 3) (towards 7)
    |> move 4 (from 5) (towards 7)
    |> move 9 (from 8) (towards 2)
    |> move 6 (from 4) (towards 1)
    |> move 2 (from 5) (towards 2)
    |> move 1 (from 7) (towards 2)
    |> move 11 (from 2) (towards 3)
    |> move 1 (from 2) (towards 1)
    |> move 7 (from 4) (towards 1)
    |> move 5 (from 6) (towards 8)
    |> move 1 (from 2) (towards 3)
    |> move 2 (from 8) (towards 7)
    |> move 14 (from 3) (towards 7)
    |> move 15 (from 7) (towards 6)
    |> move 4 (from 4) (towards 6)
    |> move 2 (from 8) (towards 3)
    |> move 12 (from 1) (towards 3)
    |> move 1 (from 8) (towards 2)
    |> move 1 (from 2) (towards 3)
    |> move 1 (from 3) (towards 9)
    |> move 1 (from 9) (towards 7)
    |> move 1 (from 1) (towards 4)
    |> move 18 (from 6) (towards 8)
    |> move 3 (from 3) (towards 2)
    |> move 17 (from 8) (towards 3)
    |> move 3 (from 7) (towards 6)
    |> move 3 (from 2) (towards 6)
    |> move 25 (from 3) (towards 7)
    |> move 2 (from 4) (towards 1)
    |> move 9 (from 6) (towards 5)
    |> move 2 (from 3) (towards 1)
    |> move 1 (from 3) (towards 9)
    |> move 5 (from 5) (towards 2)
    |> move 1 (from 8) (towards 3)
    |> move 2 (from 4) (towards 7)
    |> move 1 (from 9) (towards 4)
    |> move 1 (from 6) (towards 7)
    |> move 2 (from 5) (towards 2)
    |> move 2 (from 4) (towards 8)
    |> move 2 (from 5) (towards 8)
    |> move 5 (from 7) (towards 9)
    |> move 27 (from 7) (towards 5)
    |> move 2 (from 9) (towards 6)
let input = System.IO.File.ReadAllLines("inputs/day8.txt")

type Command = | Acc of int | Jmp of int | Nop of int

let toCommand (s : string) =
    let parts = s.Split ' '
    match parts.[0] with
    | "acc" -> Acc (parts.[1] |> int)
    | "jmp" -> Jmp (parts.[1] |> int)
    | "nop" -> Nop (parts.[1] |> int)
    | _ -> failwith "Unrecognized command"

let commands = input |> Array.map toCommand |> Array.indexed

let run (cmds : (int * Command) []) =
    let mutable accumulator = 0
    let mutable iterator = 0
    let mutable log = []

    let processCommand (i, c) =
        log <- log @ [i]
        match c with
        | Acc x -> 
            accumulator <- accumulator + x
            iterator <- iterator + 1
        | Jmp x -> iterator <- iterator + x
        | Nop _ -> iterator <- iterator + 1
        
    while (log |> List.exists ((=) iterator) |> not) do
        processCommand cmds.[iterator]

    accumulator

run commands |> printfn "%i"
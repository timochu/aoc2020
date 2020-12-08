let input = System.IO.File.ReadAllLines("inputs/day8.txt")

type Command = 
    | Acc of int
    | Jmp of int
    | Nop of int

let toCommand (s : string) =
    let parts = s.Split ' '
    match parts.[0] with
    | "acc" -> Acc (parts.[1] |> int)
    | "jmp" -> Jmp (parts.[1] |> int)
    | "nop" -> Nop (parts.[1] |> int)
    | _ -> failwith "Unrecognized command"

let commands = input |> Array.map toCommand |> Array.indexed

let mutable accumulator = 0
let mutable index = 0
let mutable log = []

let processCommand (i, c) =
    log <- log @ [i]
    printfn "accu %i" accumulator
    match c with
    | Acc x -> 
        accumulator <- accumulator + x
        index <- index + 1
    | Jmp x -> index <- index + x
    | Nop _ -> index <- index + 1

let run (cmds : (int * Command) []) =
    while (log |> List.exists ((=) index) |> not) do
        processCommand cmds.[index]
    printfn "final %i" accumulator

run commands
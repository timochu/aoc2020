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

let run subIndex (cmds : (int * Command) []) =
    let mutable log = []
    let mutable accumulator = 0
    let mutable iterator = 0

    let processCommand (commandIndex, command) =
        log <- log @ [commandIndex]
        match command with
        | Acc x -> 
            accumulator <- accumulator + x
            iterator <- iterator + 1
        | Jmp x -> 
            if commandIndex = subIndex then iterator <- iterator + 1
            else iterator <- iterator + x
        | Nop x -> 
            if commandIndex = subIndex then iterator <- iterator + x
            else iterator <- iterator + 1
    
    let isFailure () = log |> List.exists ((=) iterator)
    let isFinalInstruction () = log |> List.exists ((=) (cmds |> Array.last |> fst))

    while not (isFailure () || isFinalInstruction ()) do
        processCommand cmds.[iterator]

    (isFinalInstruction(), accumulator)

commands |> run 0 |> snd |> printfn "%i"
[0 .. commands.Length] |> Seq.map (fun x -> commands |> run x) |> Seq.pick (fun (b, r) -> if b then Some r else None ) |> printfn "%i"
        
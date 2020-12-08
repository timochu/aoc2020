let input = System.IO.File.ReadAllLines("inputs/day8.txt")

type Instruction = | Acc of int | Jmp of int | Nop of int

let toInstruction (s : string) =
    let parts = s.Split ' '
    match parts.[0] with
    | "acc" -> Acc (parts.[1] |> int)
    | "jmp" -> Jmp (parts.[1] |> int)
    | "nop" -> Nop (parts.[1] |> int)
    | _ -> failwith "Unrecognized instruction"

let instructions = input |> Array.map toInstruction |> Array.indexed

let run substitutionIndex (instructions : (int * Instruction) []) =
    let log = Array.zeroCreate instructions.Length
    let mutable accumulator = 0
    let mutable iterator = 0

    let processInstruction (index, instruction) =
        Array.set log index (Some instruction)
        match instruction with
        | Acc x -> 
            accumulator <- accumulator + x
            iterator <- iterator + 1
        | Jmp x -> 
            if index = substitutionIndex then iterator <- iterator + 1
            else iterator <- iterator + x
        | Nop x -> 
            if index = substitutionIndex then iterator <- iterator + x
            else iterator <- iterator + 1
    
    let isFailure () = log.[iterator] |> Option.isSome
    let isFinalInstruction () = iterator = instructions.Length-1

    while not (isFailure () || isFinalInstruction ()) do
        processInstruction instructions.[iterator]

    (isFinalInstruction(), accumulator)

instructions |> run 0 |> snd |> printfn "%i"
[0 .. instructions.Length] |> Seq.map (fun x -> instructions |> run x) |> Seq.pick (fun (b, r) -> if b then Some r else None ) |> printfn "%i"
        
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
    let log = Array.zeroCreate (instructions.Length + 1)
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

    let isInfiniteLoop () = log.[iterator] |> Option.isSome
    let isFinalInstruction () = iterator = instructions.Length

    while not (isInfiniteLoop() || isFinalInstruction()) do
        processInstruction instructions.[iterator]

    (accumulator, isFinalInstruction())

instructions |> run 0 |> fst |> printfn "%i"
instructions |> Seq.mapi (fun i _ -> instructions |> run i) 
             |> Seq.pick (fun (result, success) -> if success then Some result else None ) 
             |> printfn "%i"

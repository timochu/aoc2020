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
    let processInstruction index (accumulator, iterator, (log : option<Instruction> array), _) =
        let isFinalInstruction = iterator = instructions.Length
        let isInfiniteLoop = log.[iterator] |> Option.isSome
        if isInfiniteLoop || isFinalInstruction then (accumulator, iterator, log, isInfiniteLoop)
        else
            Array.set log iterator (Some (snd instructions.[iterator]))
            match snd instructions.[iterator] with
            | Acc x -> (accumulator + x, iterator + 1, log, isInfiniteLoop)
            | Jmp x -> 
                if index = substitutionIndex then (accumulator, iterator + 1, log, isInfiniteLoop)
                else (accumulator, iterator + x, log, isInfiniteLoop)
            | Nop x -> 
                if index = substitutionIndex then (accumulator, iterator + x, log, isInfiniteLoop)
                else (accumulator, iterator + 1, log, isInfiniteLoop)

    instructions 
        |> Seq.fold (fun state (x, y) -> processInstruction x state) (0, 0, Array.zeroCreate (instructions.Length + 1), false) 
        |> fun (a,_,_,b) -> (a, b)

instructions |> run -1 |> fst |> printfn "%i"
instructions |> Seq.mapi (fun i _ -> instructions |> run i) 
             |> Seq.pick (fun (result, looping) -> if not looping then Some result else None ) 
             |> printfn "%i"

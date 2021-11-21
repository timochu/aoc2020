// puzzle: https://adventofcode.com/2020/day/5

let search (sequence : string) loChar upChar = 
    let upperLimit = (pown 2 sequence.Length) - 1
    let folder (lo, hi) c = 
        let half = (hi - lo + 1) / 2
        if c = loChar then (lo, hi - half)
        elif c = upChar then (lo + half, hi)
        else failwith "Unrecognized character"
    sequence |> Seq.fold folder (0, upperLimit) |> fst

let toId (s : string) =
    let row = search s.[..6] 'F' 'B'
    let column = search s.[7..] 'L' 'R'
    row * 8 + column

let takenSeatIds = System.IO.File.ReadAllLines "inputs/day5.txt" |> Array.map toId

printfn "highest seat: %i" (takenSeatIds |> Array.max)
printfn "free seat: %A" ([|Array.min takenSeatIds .. Array.max takenSeatIds|] |> Array.except takenSeatIds)
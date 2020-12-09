let input = System.IO.File.ReadAllLines("inputs/day9.txt") |> Array.map int64
let preample = 25

let isValid index (input: int64 array) =
    let previous = input.[index-preample .. index-1]
    (previous, previous) ||> Array.allPairs |> Array.map (fun (x,y) -> x+y) |> Array.exists ((=) input.[index])

let answer = input.[preample..] |> Seq.mapi (fun i x -> (isValid (i+preample) input, x))
             |> Seq.pick (fun (valid, value) -> if not valid then Some value else None) 

printfn "%i" answer
// puzzle: https://adventofcode.com/2020/day/9
#time

let input = System.IO.File.ReadAllLines("inputs/day9.txt") |> Array.map int64
let preample = 25

let isValid index =
    let previous = input.[index-preample .. index-1]
    (previous, previous) ||> Seq.allPairs |> Seq.map (fun (x,y) -> x+y) |> Seq.exists ((=) input.[index])

let answer = 
    input.[preample..] 
    |> Seq.mapi (fun i x -> (isValid (i+preample), x))
    |> Seq.pick (fun (valid, value) -> if not valid then Some value else None) 

let permutations startIndex = 
    let indexOfAnswer = input |> Seq.findIndex ((=) answer)
    input.[startIndex..indexOfAnswer] |> Seq.scan (fun a x -> a @ [x]) []

printfn "%i" answer

seq { 0 .. input.Length } 
      |> Seq.collect permutations
      |> Seq.pick (fun a -> if List.sum a = answer then Some (List.min a + List.max a) else None) 
      |> printfn "%i"
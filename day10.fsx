// puzzle: https://adventofcode.com/2020/day/10

let input = 
    System.IO.File.ReadAllLines("inputs/day10.txt")
    |> Array.toList 
    |> List.map int 
    |> List.sort
    |> fun x -> [0] @ x @ [x |> List.max |> (+) 3]
    |> Seq.pairwise

let diffCount diff = 
    input |> Seq.where (fun (x,y) -> y-x=diff) |> Seq.length

printfn "%i" ((diffCount 1) * (diffCount 3))
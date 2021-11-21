// puzzle: https://adventofcode.com/2020/day/15
#time

let input = System.IO.File.ReadAllText("inputs/day15.txt").Split ',' |> Array.map int
let initialLength = input |> Array.length
let lookup = input.[0..initialLength-2] |> Array.indexed |> Array.map (fun (x,y) -> (y,x+1)) |> Map.ofArray
let lastInt = input |> Array.last

let nextValue i lookup prevInt =
    match lookup |> Map.containsKey prevInt with
    | false -> 0
    | true -> i - lookup.[prevInt]

let rec calc i iterations lookup prevInt =
    let next = nextValue i lookup prevInt
    match i = iterations with
    | true -> prevInt
    | false -> calc (i+1) iterations (lookup.Add(prevInt, i)) next

printfn "%i" (calc initialLength 2020 lookup lastInt)
printfn "%i" (calc initialLength 30000000 lookup lastInt)
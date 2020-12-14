let input = System.IO.File.ReadAllLines "inputs/day1.txt" |> Seq.map int

let pairs = Seq.allPairs input input
let answer = pairs |> Seq.filter (fun (x,y) -> x+y = 2020) |> Seq.head |> fun (x,y) -> x*y
printfn "%i" answer

let triples = Seq.allPairs input pairs
let answer2 = triples |> Seq.filter (fun (x, (y, z)) -> x+y+z = 2020) |> Seq.head |> fun (x, (y, z)) -> x*y*z
printfn "%i" answer2
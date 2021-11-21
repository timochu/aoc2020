let input = System.IO.File.ReadAllLines "inputs/day1.txt" |> Seq.map int

let pairs = Seq.allPairs input input
let answer = pairs |> Seq.pick (fun (x,y) -> if x+y = 2020 then Some (x*y) else None)
printfn "%i" answer

let triples = Seq.allPairs input pairs
let answer2 = triples |> Seq.pick (fun (x, (y, z)) -> if x+y+z = 2020 then Some (x*y*z) else None)
printfn "%i" answer2
let input = System.IO.File.ReadAllLines "inputs/day3.txt" |> Seq.map (String.replicate 100) |> String.concat ""
let width = System.IO.File.ReadLines "inputs/day3.txt" |> Seq.head |> String.replicate 100 |> String.length

let everyNth n =  Seq.indexed >> Seq.choose (fun (i,x) -> if (i-1) % n = n-1 then Some x else None)
let trees x y = input |> everyNth (width * y + x) |> Seq.filter ((=) '#') |> Seq.length

printfn "%i" (trees 3 1)
printfn "%i" (trees 1 1 * trees 3 1 * trees 5 1 * trees 7 1 * trees 1 2)
let input = System.IO.File.ReadAllLines "inputs/day3.txt" |> Seq.map (String.replicate 100) |> String.concat ""
let width = System.IO.File.ReadLines "inputs/day3.txt" |> Seq.head |> String.replicate 100 |> String.length

let everyNth n seq = 
  seq |> Seq.mapi (fun i el -> el, i-1)
      |> Seq.filter (fun (_, i) -> i % n = n-1)
      |> Seq.map fst

let trees x y = input |> everyNth ((width * y) + x) |> Seq.filter ((=) '#') |> Seq.length

printfn "%i" (trees 3 1)
printfn "%i" (trees 1 1 * trees 3 1 * trees 5 1 * trees 7 1 * trees 1 2)
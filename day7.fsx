let input = System.IO.File.ReadAllLines("inputs/day7.txt")
let separators = [|" contain "; " bags"; " bag"; "."; ", "; "no other"; "1 "; "2 "; "3 "; "4 "; "5 "; |]
let rules = input |> Seq.map (fun x -> x.Split(separators, System.StringSplitOptions.RemoveEmptyEntries))

let rec bagsFor bag rules =
    let initial = rules |> Seq.where (fun (x : string []) -> x.[1..] |> Seq.exists ((=) bag)) |> Seq.map Seq.head |> Seq.toList
    let recursive = initial |> Seq.collect (fun x -> bagsFor x rules) |> Seq.toList
    initial @ recursive |> List.distinct
    
printfn "%i" (rules |> bagsFor "shiny gold" |> Seq.length)
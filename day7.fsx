let input = System.IO.File.ReadAllLines("inputs/day7.txt") |> Array.toList
let separators = [| " contain "; " bags"; " bag"; "."; ", "; "no other" |]
let flattenRules (rules : list<string list>) =
    rules |> List.map (fun x -> x.[1..] |> List.collect (fun y -> List.replicate (y.[0] |> System.Char.GetNumericValue |> int) (y.[2..])) |> (fun z -> [x.[0]] @ z))
let rules =  input |> List.map (fun x -> x.Split(separators, System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList) |> flattenRules

let rec bagsFor rules bag =
    let initial = rules |> List.where (fun (x : string list) -> x.[1..] |> Seq.exists ((=) bag)) |> List.map Seq.head
    let recursive = initial |> List.collect (bagsFor rules)
    initial @ recursive |> List.distinct

let rec bagsIn (rules : list<string list>) (bag : string) =
    let initial = rules |> List.find (fun x -> x.[0] = bag) |> fun x -> x.[1..]
    let recursive = initial |> List.collect (bagsIn rules)
    initial @ recursive

"shiny gold" |> bagsFor rules |> List.length |> printfn "%i"
"shiny gold" |> bagsIn rules |> List.length |> printfn "%i"
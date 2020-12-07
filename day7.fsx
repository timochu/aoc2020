let input = System.IO.File.ReadAllLines("inputs/day7.txt") |> Array.toList

let splitRules (str : string) = 
    str.Split ([|" contain "; " bags"; " bag"; "."; ", "; "no other"|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let flattenRule (rule : string list) =
    rule.[1..] 
    |> List.collect (fun color -> List.replicate (color.[0] |> System.Char.GetNumericValue |> int) (color.[2..])) 
    |> fun colors -> rule.[0] :: colors

let rules = input |> List.map (splitRules >> flattenRule)

let rec bagsFor color =
    let result = rules |> List.where (fun rule -> rule.[1..] |> List.exists ((=) color)) |> List.map List.head
    result @ (result |> List.collect bagsFor) |> List.distinct

let rec bagsIn color =
    let initial = rules |> List.find (fun rule -> rule.[0] = color) |> fun x -> x.[1..]
    initial @ (initial |> List.collect bagsIn)

"shiny gold" |> bagsFor |> List.length |> printfn "%i"
"shiny gold" |> bagsIn |> List.length |> printfn "%i"
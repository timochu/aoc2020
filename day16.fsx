open System
open type System.Environment
open type System.StringSplitOptions

let input =  System.IO.File.ReadAllText("inputs/day16.txt").Split(NewLine + NewLine)

let rules = 
    input 
    |> Seq.head 
    |> fun x -> x.Split([|" "; NewLine|], RemoveEmptyEntries) 
    |> Seq.where (fun x -> x.Contains '-')
    |> Seq.map (fun x -> x.Split '-')
    |> Seq.map (fun x -> (x |> Seq.head |> int, x |> Seq.last |> int))

let nearbyValues =
    input
    |> Seq.last
    |> fun x -> x.Split NewLine
    |> Seq.skip 1
    |> Seq.collect (fun x -> x.Split([|","; NewLine|], RemoveEmptyEntries))
    |> Seq.map int

let isBetween value (min, max) = value >= min && value <= max
let isWithinRules value = rules |> Seq.exists (isBetween value)
let invalidValues values = values |> Seq.fold (fun state x -> if isWithinRules x then state else x :: state) []

printfn "%A" (nearbyValues |> invalidValues |> Seq.sum)
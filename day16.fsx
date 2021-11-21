// puzzle: https://adventofcode.com/2020/day/16

open System
open type System.Environment

let input =  System.IO.File.ReadAllText("inputs/day16.txt").Split(NewLine + NewLine)

let split (by : string seq) (s : string) = s.Split(by |> Seq.toArray, StringSplitOptions.RemoveEmptyEntries)

let rules = 
    input 
    |> Seq.head 
    |> split [" "; NewLine]
    |> Seq.where (fun x -> x.Contains '-')
    |> Seq.map (split ["-"] >> fun x -> (x |> Seq.head |> int, x |> Seq.last |> int))

let nearbyValues =
    input
    |> Seq.last
    |> fun x -> x.Split NewLine
    |> Seq.skip 1
    |> Seq.collect (split [","; NewLine])
    |> Seq.map int

let isBetween value (min, max) = value >= min && value <= max
let isWithinRules value = rules |> Seq.exists (isBetween value)
let invalidValues values = values |> Seq.fold (fun state x -> if isWithinRules x then state else x :: state) []

printfn "%A" (nearbyValues |> invalidValues |> Seq.sum)
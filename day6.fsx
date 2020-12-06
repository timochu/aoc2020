open type System.Environment
open type System.Char

let split (splitter : string) (s : string) = s.Split splitter
let groups = System.IO.File.ReadAllText "inputs/day6.txt" |> split (NewLine + NewLine)

groups |> Array.sumBy (Seq.distinct >> Seq.where IsLetter >> Seq.length)  |> printfn "Answer 1: %i"
groups |> Array.sumBy (split NewLine >> Seq.map Set.ofSeq >> Set.intersectMany >> Set.count) |> printfn "Answer 2: %i"
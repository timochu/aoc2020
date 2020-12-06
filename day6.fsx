let nl = System.Environment.NewLine
let split (splitter : string) (s : string) = s.Split splitter
let groups = System.IO.File.ReadAllText "inputs/day6.txt" |> split (nl + nl)

groups |> Array.sumBy (Seq.distinct >> Seq.where System.Char.IsLetter >> Seq.length)  |> printfn "Answer 1: %i"
groups |> Array.sumBy (split nl >> Seq.map Set.ofSeq >> Set.intersectMany >> Set.count) |> printfn "Answer 2: %i"
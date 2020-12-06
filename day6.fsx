open type System.Environment

let splitBy (splitter : string) (s : string) = s.Split splitter

let groups = System.IO.File.ReadAllText "inputs/day6.txt" |> splitBy (NewLine + NewLine)
groups |> Array.sumBy (Seq.distinct >> Seq.where System.Char.IsLetter >> Seq.length)  |> printfn "Answer 1: %i"
groups |> Array.sumBy (splitBy NewLine >> Seq.map Set.ofSeq >> Set.intersectMany >> Set.count) |> printfn "Answer 2: %i"
let nl = System.Environment.NewLine
let groups = System.IO.File.ReadAllText "inputs/day6.txt" |> (fun x -> x.Split(nl + nl))

groups |> Array.sumBy (Seq.distinct >> Seq.where System.Char.IsLetter >> Seq.length)  |> printfn "Answer 1: %i"
groups |> Array.sumBy (fun group -> group.Split nl |> Seq.map Set.ofSeq |> Set.intersectMany |> Set.count) |> printfn "Answer 2: %i"
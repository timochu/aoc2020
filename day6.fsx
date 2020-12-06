open type System.Environment

let groups = (System.IO.File.ReadAllText "inputs/day6.txt").Split(NewLine + NewLine)
groups |> Array.sumBy (Seq.distinct >> Seq.where System.Char.IsLetter >> Seq.length)  |> printfn "%i"
groups |> Array.sumBy (fun x -> x.Split NewLine |> Seq.map Set.ofSeq |> Set.intersectMany |> Set.count) |> printfn "%i"
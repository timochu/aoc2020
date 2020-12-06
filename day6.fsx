open System
let groups = System.IO.File.ReadAllText "inputs/day6.txt" |> (fun x -> x.Split(Environment.NewLine + Environment.NewLine))

groups |> Array.sumBy (Seq.distinct >> Seq.where System.Char.IsLetter >> Seq.length)  |> printfn "%i"
groups |> Array.sumBy (fun x -> x.Split(Environment.NewLine) |> Seq.map Set.ofSeq |> Set.intersectMany |> Set.count) |> printfn "%i"
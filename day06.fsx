// puzzle: https://adventofcode.com/2020/day/6

open System
let groups = IO.File.ReadAllText("inputs/day6.txt").Split(Environment.NewLine + Environment.NewLine)
groups |> Array.sumBy (Seq.distinct >> Seq.where Char.IsLetter >> Seq.length)  |> printfn "%i"
groups |> Array.sumBy (fun x -> x.Split Environment.NewLine |> Seq.map Set.ofSeq |> Set.intersectMany |> Set.count) |> printfn "%i"
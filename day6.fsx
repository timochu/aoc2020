let groups = System.IO.File.ReadAllText "inputs/day6.txt" |> (fun x -> x.Split(System.Environment.NewLine + System.Environment.NewLine))

groups |> Array.sumBy (Seq.toArray >> Array.distinct >> Array.where System.Char.IsLetter >> Array.length)  |> printfn "%i"

let countSameAnswers group =
    let persons = group |> Array.tryFind (fst >> System.Char.IsWhiteSpace) |> Option.map snd |> Option.defaultValue 0 |> (+) 1
    group |> Array.where (fun (_,y) -> y = persons) |> Array.length

groups |> Array.sumBy (Seq.toArray >> Array.countBy id >> countSameAnswers) |> printfn "%i"
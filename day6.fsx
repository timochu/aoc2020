let groups = System.IO.File.ReadAllText "inputs/day6.txt" |> (fun x -> x.Split(System.Environment.NewLine + System.Environment.NewLine))

groups |> Array.sumBy (Seq.distinct >> Seq.where System.Char.IsLetter >> Seq.length)  |> printfn "%i"

let countSameAnswers group =
    let persons = group |> Seq.tryFind (fst >> System.Char.IsWhiteSpace) |> Option.map snd |> Option.defaultValue 0 |> (+) 1
    group |> Seq.where (fun (_,y) -> y = persons) |> Seq.length

groups |> Array.sumBy (Seq.countBy id >> countSameAnswers) |> printfn "%i"
let input =  System.IO.File.ReadAllText("inputs/day15.txt").Split ',' |> Seq.map int |> Seq.toList

let nextValue (a: list<'a>) =
    let prevInt = a |> Seq.last
    match a.[..a.Length-2] |> Seq.contains prevInt with
    | false -> 0
    | true ->
        let prevIndex = a |> Seq.findIndexBack ((=) prevInt)
        let prevIndex2 = a.[..a.Length-2] |> Seq.findIndexBack ((=) prevInt)
        prevIndex-prevIndex2

let calc iterations =
    [0 .. iterations-input.Length-1] |> Seq.fold (fun state _ -> state @ [nextValue state]) input

printfn "%i" (calc 2020 |> List.last)
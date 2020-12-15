#time
let input = System.IO.File.ReadAllText("inputs/day15.txt").Split ',' |> Seq.map int |> Seq.rev
let initialLength = input |> Seq.length

let nextValue a =
    let prevInt = a |> Seq.head
    match a |> Seq.skip 1 |> Seq.exists ((=) prevInt) with
    | false -> 0
    | true -> a |> Seq.skip 1 |> Seq.findIndex ((=) prevInt) |> (+) 1

let rec calc iterations a =
    let next = nextValue a
    match iterations with
    | 0 -> a
    | _ ->
        let combined = seq {
            yield next
            yield! a
        }
        calc (iterations-1) combined

printfn "%i" (calc (2020-initialLength) input |> Seq.head)
// printfn "%i" (calc (30000000-initialLength) input |> Seq.head)

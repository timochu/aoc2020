let input =  System.IO.File.ReadAllLines("inputs/day12.txt")

type Action = 
| North of int 
| South of int 
| East of int
| West of int
| Left of int
| Right of int
| Forward of int

let toAction (s : string) =
    match s.[0], s.[1..] |> int with
    | 'N', x -> North x
    | 'S', x -> South x
    | 'E', x -> East x
    | 'W', x -> West x
    | 'L', x -> Left x
    | 'R', x -> Right x
    | 'F', x -> Forward x
    | x -> failwithf "Unrecognized action: %A" x

let actions = input |> Array.map toAction
let add a b =
    let result = a+b
    if result > 360 then result - 360
    else if result < 0 then result + 360
    else result

let position =
    actions 
    |> Array.fold (fun (x,y,d) action -> 
        printfn "%i %i %i" x y d
        match action with
        | North v -> (x,y+v,d)
        | South v -> (x,y-v,d)
        | East v -> (x+v,y,d)
        | West v -> (x-v,y,d)
        | Left v -> (x,y, add d -v)
        | Right v -> (x,y, add d +v)
        | Forward v ->
            match d with
            | 0 -> (x,y+v,d)
            | 90 -> (x+v,y,d)
            | 180 -> (x,y-v,d)
            | 270 -> (x-v,y,d)
            | 360 -> (x,y+v,d)
        ) (0, 0, 90)

let manhattanDistance (x,y,_) = (abs x) + (abs y)

actions |> Array.iter (printfn "%A")
printfn "%A" (manhattanDistance position)
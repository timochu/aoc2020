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
let manhattanDistance (x,y,_) = (abs x) + (abs y)
let wrap =
    function
    | x when x > 360 -> x - 360
    | x when x < 0 -> x + 360
    | x -> x

actions 
|> Array.fold (fun (x,y,d) action -> 
    match action with
    | North v -> (x,y+v,d)
    | South v -> (x,y-v,d)
    | East v -> (x+v,y,d)
    | West v -> (x-v,y,d)
    | Left v -> (x,y, d-v |> wrap)
    | Right v -> (x,y, d+v |> wrap)
    | Forward v ->
        match d with
        | 0 -> (x,y+v,d)
        | 90 -> (x+v,y,d)
        | 180 -> (x,y-v,d)
        | 270 -> (x-v,y,d)
        | 360 -> (x,y+v,d)
        | _ -> failwith "Unrecognized direction"
    ) (0, 0, 90)
    |> fun (x,y,_) -> (abs x) + (abs y)
    |> printfn "%A"

let rot90 (x, y, x2, y2) = (y,-x,x2,y2)

actions 
|> Array.fold (fun (x,y,x2,y2) action -> 
    match action with
    | North v -> (x,y+v,x2,y2)
    | South v -> (x,y-v,x2,y2)
    | East v -> (x+v,y,x2,y2)
    | West v -> (x-v,y,x2,y2)
    | Left v ->
        match v with
        | 90 -> (x,y,x2,y2) |> rot90 |> rot90 |> rot90
        | 180 -> (x,y,x2,y2) |> rot90 |> rot90
        | 270 -> (x,y,x2,y2) |> rot90
        | _ -> failwith "Unrecognized direction"
    | Right v ->
        match v with
        | 90 -> (x,y,x2,y2) |> rot90 
        | 180 -> (x,y,x2,y2) |> rot90 |> rot90
        | 270 -> (x,y,x2,y2) |> rot90 |> rot90 |> rot90
        | _ -> failwith "Unrecognized direction"
    | Forward v -> (x,y,x2+(x*v),y2+(y*v))
    ) (10, 1, 0, 0)
    |> fun (_,_,x,y) -> (abs x) + (abs y)
    |> printfn "%A"
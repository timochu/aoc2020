let input =  System.IO.File.ReadAllLines("inputs/day12.txt")

let manhattanDistance (x,y,_) = (abs x) + (abs y)
let rot90 (x, y, x2, y2) = (y,-x,x2,y2)
let wrap =
    function
    | x when x > 360 -> x - 360
    | x when x < 0 -> x + 360
    | x -> x

input 
|> Array.fold (fun (x,y,d) action -> 
    match action.[0], action.[1..] |> int with
    | 'N', v -> (x,y+v,d)
    | 'S', v -> (x,y-v,d)
    | 'E', v -> (x+v,y,d)
    | 'W', v -> (x-v,y,d)
    | 'L', v -> (x,y, d-v |> wrap)
    | 'R', v -> (x,y, d+v |> wrap)
    | 'F', v ->
        match d with
        | 0 -> (x,y+v,d)
        | 90 -> (x+v,y,d)
        | 180 -> (x,y-v,d)
        | 270 -> (x-v,y,d)
        | 360 -> (x,y+v,d)
        | _ -> failwith "Unrecognized direction"
    | _ -> failwith "Unrecognized action"
    ) (0, 0, 90)
    |> fun (x,y,_) -> (abs x) + (abs y)
    |> printfn "%A"

input 
|> Array.fold (fun (x,y,x2,y2) action -> 
    match action.[0], action.[1..] |> int with
    | 'N', v -> (x,y+v,x2,y2)
    | 'S', v -> (x,y-v,x2,y2)
    | 'E', v -> (x+v,y,x2,y2)
    | 'W', v -> (x-v,y,x2,y2)
    | 'L', v ->
        match v with
        | 90 -> (x,y,x2,y2) |> rot90 |> rot90 |> rot90
        | 180 -> (x,y,x2,y2) |> rot90 |> rot90
        | 270 -> (x,y,x2,y2) |> rot90
        | _ -> failwith "Unrecognized direction"
    | 'R', v ->
        match v with
        | 90 -> (x,y,x2,y2) |> rot90 
        | 180 -> (x,y,x2,y2) |> rot90 |> rot90
        | 270 -> (x,y,x2,y2) |> rot90 |> rot90 |> rot90
        | _ -> failwith "Unrecognized direction"
    | 'F', v -> (x,y,x2+(x*v),y2+(y*v))
    | _ -> failwith "Unrecognized action"
    ) (10, 1, 0, 0)
    |> fun (_,_,x,y) -> (abs x) + (abs y)
    |> printfn "%A"
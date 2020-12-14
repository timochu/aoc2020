let input =  System.IO.File.ReadAllLines("inputs/day11.txt") |> array2D
let l1 = Array2D.length1 input - 1
let l2 = Array2D.length2 input - 1

let rec look row col (seats : char[,]) depth maxDepth rowDir colDir =
    match depth, row+rowDir, col+colDir with
    | d, _, _ when d = maxDepth -> None
    | _, r, _ when r < 0 -> None
    | _, r, _ when r > l1 -> None
    | _, _, c when c < 0 -> None
    | _, _, c when c > l2 -> None
    | d, r, c when seats.[r,c] = '.' -> look r c seats (d+1) maxDepth rowDir colDir
    | _, r, c -> Some seats.[r, c]

let neighbors row col lookDistance seats =
    let looker = look row col seats 0 lookDistance
    [yield looker -1 0
     yield looker +1 0
     yield looker 0 -1
     yield looker 0 +1
     yield looker -1 -1
     yield looker -1 +1
     yield looker +1 +1
     yield looker +1 -1] |> Seq.choose id

let seat row col maxNeighbours lookDistance (a : char[,]) =
    match a.[row,col], a with
    | 'L', a when a |> neighbors row col lookDistance |> Seq.where ((=) '#') |> Seq.isEmpty -> '#'
    | '#', a when a |> neighbors row col lookDistance |> Seq.where ((=) '#') |> Seq.length > maxNeighbours -> 'L'
    | x, _ -> x

let rec finalSeatingCount seats occ maxNeighbours lookDistance =
    let doSeating max depth a = a |> Array2D.mapi (fun r c _ -> seat r c max depth a)
    let occupiedSeats a = a |> Seq.cast |> Seq.where ((=) '#') |> Seq.length
    let result = seats |> doSeating maxNeighbours lookDistance
    let occupied = result |> occupiedSeats 
    if occ = occupied then occupied else finalSeatingCount result occupied maxNeighbours lookDistance

printfn "%A" (finalSeatingCount input 0 3 1)
printfn "%A" (finalSeatingCount input 0 4 ([l1; l2] |> List.max))
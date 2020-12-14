let input =  System.IO.File.ReadAllLines("inputs/day11.txt") |> array2D
let l1 = Array2D.length1 input - 1
let l2 = Array2D.length2 input - 1

let neighbors r c a =
    let l1 = Array2D.length1 a - 1
    let l2 = Array2D.length2 a - 1
    [if r > 0 then yield a.[r-1,c]
     if r < l1 then yield a.[r+1,c]
     if c > 0 then yield a.[r,c-1]
     if c < l2 then yield a.[r,c+1]
     if r > 0 && c > 0 then yield a.[r-1,c-1]
     if r > 0 && c < l2 then yield a.[r-1,c+1]
     if r < l1 && c < l2 then yield a.[r+1,c+1]
     if r < l1 && c > 0 then yield a.[r+1,c-1]]

let rec look r c (a : char[,]) rd cd =
    if r+rd < 0 || c+cd < 0 || r+rd > l1 || c+cd > l2 then None
    else
        if a.[r+rd,c+cd] = '.' then look (r+rd) (c+cd) a rd cd
        else Some a.[r+rd,c+cd]

let neighbors2 r c a =
    let looker = look r c a
    [yield looker -1 0
     yield looker +1 0
     yield looker 0 -1
     yield looker 0 +1
     yield looker -1 -1
     yield looker -1 +1
     yield looker +1 +1
     yield looker +1 -1] |> Seq.choose id

let seat r c (a : char[,]) =
    match a.[r,c], a with
    | '.', _ -> '.'
    | 'L', a when a |> neighbors2 r c |> Seq.where ((=) '#') |> Seq.isEmpty -> '#'
    | '#', a when a |> neighbors2 r c |> Seq.where ((=) '#') |> Seq.length > 4 -> 'L'
    | x, _ -> x

let doSeats a = a |> Array2D.mapi (fun r c _ -> seat r c a)
let occupiedSeats a = a |> Seq.cast |> Seq.where ((=) '#') |> Seq.length

let rec finalSeatingCount seats occ =
    let result = seats |> doSeats
    let occupied = result |> occupiedSeats
    if occ = occupied then occupied else finalSeatingCount result occupied

printfn "%A" (finalSeatingCount input 0)
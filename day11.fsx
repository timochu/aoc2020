let input =  System.IO.File.ReadAllLines("inputs/day11.txt") |> array2D

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

let isAvailable r c (a : char[,]) = 
    let s = a.[r,c]
    if s = 'L' then a |> neighbors r c |> Seq.where ((=) '#') |> Seq.isEmpty
    else a |> neighbors r c |> Seq.where ((=) '#') |> Seq.length < 4

let seat r c (a : char[,]) =
    let s = a.[r,c]
    if s = '.' then '.' else if isAvailable r c a then '#' else 'L'

let doSeats a = a |> Array2D.mapi (fun r c _ -> seat r c a)

let occupiedSeats a = a |> Seq.cast |> Seq.where ((=) '#') |> Seq.length

let finalSeating =
    [0 .. 140]
    |> Seq.fold (fun state _ -> state |> doSeats) input

printfn "%A" (occupiedSeats finalSeating)
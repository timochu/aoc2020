let search (s : string) c = 
    s 
    |> Seq.fold (fun (lo, hi, _) x -> if x=c then (lo, hi-(hi+1-lo)/2, lo) else (lo+(hi+1-lo)/2, hi, hi)) (0, (pown 2 s.Length)-1, 0) 
    |> fun (_,_,a) -> a

let toId (s : string) =
    let row = search s.[..6] 'F'
    let column = search s.[7..] 'L'
    row * 8 + column

let input = System.IO.File.ReadAllLines "inputs/day5.txt" |> Array.map toId |> Array.sortBy id

printfn "highest seat: %i" (input |> Array.last)
printfn "free seat: %i" ([Array.head input .. Array.last input] |> List.except input |> List.last)
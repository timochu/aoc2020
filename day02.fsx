type Password = 
    { Min: int
      Max: int
      Character: char
      Pass: string }

let toPassword (s: string) =
    let parts = s.Split(' ', '-', ':')
    { Min = parts.[0] |> int
      Max = parts.[1] |> int
      Character = parts.[2] |> char
      Pass = parts.[4] }

let isValid password = 
    let count = password.Pass |> Seq.where ((=) password.Character) |> Seq.length
    password.Min <= count && count <= password.Max

let isValid2 password = 
    (password.Pass.[password.Min-1] = password.Character) <> (password.Pass.[password.Max-1] = password.Character)

let passwords = System.IO.File.ReadAllLines "inputs/day2.txt" |> Seq.map toPassword

printfn "%i" (passwords |> Seq.where isValid |> Seq.length)
printfn "%i" (passwords |> Seq.where isValid2 |> Seq.length)
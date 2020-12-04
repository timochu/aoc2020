open System

type Passport = 
    { Byr: int option
      Iyr: int option
      Eyr: int option
      Hgt: (int * string) option
      Hcl: string option
      Ecl: string option
      Pid: string option
      Cid: string option }

let toPassport (s : string) =
    let parts = s.Split([|" "; ":"; Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    let getEntry s = parts |> Array.tryFindIndex ((=) s) |> Option.map ((+) 1 >> Array.get parts)
    let toHeight s = (s |> Seq.where Char.IsNumber |> String.Concat |> int, s |> Seq.where Char.IsLetter |> String.Concat)
    { Byr = getEntry "byr" |> Option.map int
      Iyr = getEntry "iyr" |> Option.map int
      Eyr = getEntry "eyr" |> Option.map int
      Hgt = getEntry "hgt" |> Option.map toHeight
      Hcl = getEntry "hcl"
      Ecl = getEntry "ecl"
      Pid = getEntry "pid"
      Cid = getEntry "cid" }

let isValid p = 
    p.Byr.IsSome && p.Iyr.IsSome && p.Eyr.IsSome && p.Hgt.IsSome && p.Hcl.IsSome && p.Ecl.IsSome && p.Pid.IsSome

let isValid2 p = 
    let between min max x = min <= x && x <= max
    let hasValidByr p = p.Byr |> Option.exists (between 1920 2002)
    let hasValidIyr p = p.Iyr |> Option.exists (between 2010 2020)
    let hasValidEyr p = p.Eyr |> Option.exists (between 2020 2030)
    let hasValidHgt p = 
        match p.Hgt with
        | Some (value, unit) when unit = "cm" -> between 150 193 value
        | Some (value, unit) when unit = "in" -> between 59 76 value
        | _ -> false
    let hasValidHcl p = p.Hcl |> Option.exists (fun x -> x.[0] = '#' && x |> Seq.where Char.IsLetterOrDigit |> Seq.length |> (=) 6)
    let hasValidEcl p = p.Ecl |> Option.exists (fun x -> ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.contains x)
    let hasValidPid p = p.Pid |> Option.exists (fun x -> x |> Seq.where Char.IsNumber |> Seq.length |> (=) 9)
    hasValidByr p && hasValidIyr p && hasValidEyr p && hasValidHgt p && hasValidHcl p && hasValidEcl p && hasValidPid p

let input = IO.File.ReadAllText "inputs/day4.txt" |> (fun x -> x.Split(Environment.NewLine |> String.replicate 2)) |> Seq.map toPassport

printfn "%i" (input |> Seq.where isValid |> Seq.length)
printfn "%i" (input |> Seq.where isValid2 |> Seq.length)
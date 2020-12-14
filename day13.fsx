let input =  System.IO.File.ReadAllLines("inputs/day13.txt")
let timestamp = input.[0] |> int
let buses = input.[1].Split ',' |> Seq.where ((<>) "x") |> Seq.map int |> Seq.toArray

let rec addWhileUnder x aggregate =
    if (aggregate+x) < timestamp then addWhileUnder x (aggregate+x)
    else (aggregate+x)

let waitTimes = buses |> Seq.map (fun x -> addWhileUnder x 0 |> (-) timestamp)
let shortestWait = waitTimes |> Seq.max
let bus = waitTimes |> Seq.findIndex ((=) shortestWait) |> fun x -> buses.[x]

printfn "%i" (bus * (abs shortestWait))
let input =  System.IO.File.ReadAllLines("inputs/day13.txt")
let timestamp = input.[0] |> int64
let buses = input.[1].Split ',' |> Seq.indexed |> Seq.where (fun (_, x) -> x <> "x") |> Seq.map (fun (i,x) -> (int64 i, int64 x)) |> Seq.toArray

let rec addWhileUnder x aggregate =
    if (aggregate+x) < timestamp then addWhileUnder x (aggregate+x)
    else (aggregate+x)

let waitTimes = buses |> Seq.map (fun (_, x) -> addWhileUnder x 0L |> (-) timestamp)
let shortestWait = waitTimes |> Seq.max
let bus = waitTimes |> Seq.findIndex ((=) shortestWait) |> fun x -> buses.[x] |> snd

printfn "%i" (bus * (abs shortestWait))

let mutable t = 0L
let mutable found = false
let checker (i, bus) = ((t+i) % bus) = 0L
while not found do
    t <- t+1L
    found <- buses |> Seq.forall checker

printfn "%i" t

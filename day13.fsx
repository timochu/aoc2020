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

let busCount = buses.Length - 1
let lcm a = a |> Seq.fold (*) 1L // only works for prime numbers

let rec findTime busIndex increment t =
    let result = buses.[..busIndex] |> Seq.forall (fun (i, bus) -> (t+i) % bus = 0L)
    match result, busIndex = busCount with
    | false, _ -> findTime busIndex increment (t+increment)
    | true, false -> findTime (busIndex+1) ((buses.[..busIndex] |> Seq.map snd |> lcm)) t
    | true, true -> t

printfn "%i" (findTime 0 1L 0L)
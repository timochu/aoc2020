let input =  System.IO.File.ReadAllLines("inputs/day13.txt")
let timestamp = input.[0] |> int64
let buses = 
    input.[1].Split ',' |> Array.indexed |> Array.where (snd >> (<>) "x") |> Array.map (fun (i,x) -> (int64 i, int64 x))

let rec waitTime aggregate x =
    let result = aggregate + x
    match result < timestamp with
    | true -> waitTime result x
    | false -> result

let bus = buses |> Seq.map (fun (_ ,id) -> (id, id |> waitTime 0L |> (-) timestamp |> abs)) |> Seq.minBy snd
printfn "%i" (bus |> fun (id, wait) -> id * wait)

let busCount = buses.Length - 1
let lcm a = a |> Seq.reduce (*) // only works for prime numbers

let rec findTime busIndex increment t =
    let result = buses.[..busIndex] |> Seq.forall (fun (i, bus) -> (t+i) % bus = 0L)
    match result, busIndex = busCount with
    | false, _ -> findTime busIndex increment (t+increment)
    | true, false -> findTime (busIndex+1) ((buses.[..busIndex] |> Seq.map snd |> lcm)) t
    | true, true -> t

printfn "%i" (findTime 0 1L 0L)
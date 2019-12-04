module Day04

let input = [246540..787419] |> Seq.map (string >> (fun x -> x.ToCharArray()))

let isIncreasing = Seq.pairwise >> Seq.forall (fun (x,y) -> x <= y)

module Part1 =
    let hasTwins = Seq.pairwise >> Seq.exists (fun (x,y) -> x = y)

    input
    |> Seq.filter isIncreasing
    |> Seq.filter hasTwins
    |> Seq.length
    |> printfn "%i"

module Part2 =
    let rec hasNonClusterTwins = function
    | x::y::z::xs when x = y && y = z -> hasNonClusterTwins (y::z::xs)
    | x::y::_ when x = y -> true
    | _::xs -> hasNonClusterTwins xs
    | [] -> false

    input
    |> Seq.filter isIncreasing
    |> Seq.filter (List.ofArray >> hasNonClusterTwins)
    |> Seq.length
    |> printfn "%i"
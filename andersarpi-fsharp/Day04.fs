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
    let hasNonClusterTwins (xs: char list) =
        let rec run l prev =
            match l with
            | x::y::z::_ when x = y && x <> z && x <> prev -> true
            | x::y::[] when x = y && x <> prev -> true
            | x::tail -> run tail x
            | [] -> false

        run xs ' '

    input
    |> Seq.filter isIncreasing
    |> Seq.filter (List.ofArray >> hasNonClusterTwins)
    |> Seq.length
    |> printfn "%i"
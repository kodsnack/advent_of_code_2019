module Day08

let part1 () = 
    let input = System.IO.File.ReadAllText("day08input.txt")
    let layer = input.ToCharArray()
                |> Seq.chunkBySize 150
                |> Seq.map (Seq.countBy id >> Map.ofSeq)
                |> Seq.minBy (Map.find '0')

    layer.['1'] * layer.['2'] |> printfn "%i"

let part2 () =
    let input = System.IO.File.ReadAllText("day08input.txt")
    let layers = input.ToCharArray() |> Seq.chunkBySize 150

    let image = [
        for i in 0..149 ->
            layers
            |> Seq.find (fun l -> l.[i] <> '2')
            |> (fun l -> if l.[i] = '1' then '█' else ' ')
    ]

    image
    |> Seq.chunkBySize 25
    |> Seq.iter (System.String >> printfn "%s")

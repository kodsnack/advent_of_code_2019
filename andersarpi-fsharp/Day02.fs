module Day02

let input = [|
    1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;10;19;2;9;19;23;1;9;23;27;2;27;9;31;1;31;5;35;2;35;9;
    39;1;39;10;43;2;43;13;47;1;47;6;51;2;51;10;55;1;9;55;59;2;6;59;63;1;63;6;67;1;67;10;71;1;
    71;10;75;2;9;75;79;1;5;79;83;2;9;83;87;1;87;9;91;2;91;13;95;1;95;9;99;1;99;6;103;2;103;6;
    107;1;107;5;111;1;13;111;115;2;115;6;119;1;119;5;123;1;2;123;127;1;6;127;0;99;2;14;0;0
|]

let runIntCodes (reg: int[]) =
    [0..4..reg.Length]
    |> Seq.tryFind (fun i ->
        let opCode = reg.[i]
        if opCode = 99 then true else
            let i1 = reg.[i+1]
            let i2 = reg.[i+2]
            let out = reg.[i+3]
            let op = match opCode with
                        | 1 -> (+)
                        | 2 -> (*)
                        | _ -> failwith "incorrect opCode"
            reg.[out] <- op reg.[i1] reg.[i2]
            false
    ) |> ignore
    reg

//part1
let day1input = Array.copy input
day1input.[1] <- 12
day1input.[2] <- 2
runIntCodes day1input |> Array.item 0 |> printfn "%O"

//part2
[for n in [0..100] do for v in [0..100] do n, v]
|> Seq.tryFind (fun (n,v) ->
    let day2input = Array.copy input
    day2input.[1] <- n
    day2input.[2] <- v
    let result = runIntCodes day2input |> Array.item 0
    result = 19690720
)
|> Option.map (printfn "%O")
|> ignore
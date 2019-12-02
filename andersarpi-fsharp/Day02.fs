module Day02

let input = [|
    1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;10;19;2;9;19;23;1;9;23;27;2;27;9;31;1;31;5;35;2;35;9;
    39;1;39;10;43;2;43;13;47;1;47;6;51;2;51;10;55;1;9;55;59;2;6;59;63;1;63;6;67;1;67;10;71;1;
    71;10;75;2;9;75;79;1;5;79;83;2;9;83;87;1;87;9;91;2;91;13;95;1;95;9;99;1;99;6;103;2;103;6;
    107;1;107;5;111;1;13;111;115;2;115;6;119;1;119;5;123;1;2;123;127;1;6;127;0;99;2;14;0;0
|]

let getOp = function
| 1 -> (+)
| 2 -> (*)
| _ -> failwith "incorrect op code"

let getInstructions (reg: int[]) i =
    match reg.[i] with
    | 99 -> [99]
    | op -> [op; reg.[i+1]; reg.[i+2]; reg.[i+3]]

let runIntCodes (register: int[]) =
    let rec run (reg: int[]) ixs =
        match ixs with
        | i::tail ->
            match getInstructions reg i with
            | [99] -> reg
            | [opCode; i1; i2; out] ->
                reg.[out] <- (getOp opCode) reg.[i1] reg.[i2]
                run reg tail
            | _ -> failwith "incorrect instruction set"
        | _ -> failwith "failed to halt"
    run register [0..4..register.Length]

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
module Day05

let input = [|
    3;225;1;225;6;6;1100;1;238;225;104;0;1101;72;36;225;1101;87;26;225;2;144;13;224;101;-1872;224;
    224;4;224;102;8;223;223;1001;224;2;224;1;223;224;223;1102;66;61;225;1102;25;49;224;101;-1225;224;
    224;4;224;1002;223;8;223;1001;224;5;224;1;223;224;223;1101;35;77;224;101;-112;224;224;4;224;102;
    8;223;223;1001;224;2;224;1;223;224;223;1002;195;30;224;1001;224;-2550;224;4;224;1002;223;8;223;
    1001;224;1;224;1;224;223;223;1102;30;44;225;1102;24;21;225;1;170;117;224;101;-46;224;224;4;224;
    1002;223;8;223;101;5;224;224;1;224;223;223;1102;63;26;225;102;74;114;224;1001;224;-3256;224;4;
    224;102;8;223;223;1001;224;3;224;1;224;223;223;1101;58;22;225;101;13;17;224;101;-100;224;224;4;
    224;1002;223;8;223;101;6;224;224;1;224;223;223;1101;85;18;225;1001;44;7;224;101;-68;224;224;4;
    224;102;8;223;223;1001;224;5;224;1;223;224;223;4;223;99;0;0;0;677;0;0;0;0;0;0;0;0;0;0;0;1105;
    0;99999;1105;227;247;1105;1;99999;1005;227;99999;1005;0;256;1105;1;99999;1106;227;99999;1106;
    0;265;1105;1;99999;1006;0;99999;1006;227;274;1105;1;99999;1105;1;280;1105;1;99999;1;225;225;
    225;1101;294;0;0;105;1;0;1105;1;99999;1106;0;300;1105;1;99999;1;225;225;225;1101;314;0;0;106;
    0;0;1105;1;99999;7;677;226;224;102;2;223;223;1005;224;329;101;1;223;223;8;677;226;224;1002;
    223;2;223;1005;224;344;1001;223;1;223;1107;677;677;224;102;2;223;223;1005;224;359;1001;223;1;
    223;1107;226;677;224;102;2;223;223;1005;224;374;101;1;223;223;7;226;677;224;102;2;223;223;1005;
    224;389;101;1;223;223;8;226;677;224;1002;223;2;223;1005;224;404;101;1;223;223;1008;226;677;224;
    1002;223;2;223;1005;224;419;1001;223;1;223;107;677;677;224;102;2;223;223;1005;224;434;101;1;223;
    223;1108;677;226;224;1002;223;2;223;1006;224;449;101;1;223;223;1108;677;677;224;102;2;223;223;
    1006;224;464;101;1;223;223;1007;677;226;224;102;2;223;223;1006;224;479;101;1;223;223;1008;226;
    226;224;102;2;223;223;1006;224;494;101;1;223;223;108;226;226;224;1002;223;2;223;1006;224;509;
    101;1;223;223;107;226;226;224;102;2;223;223;1006;224;524;101;1;223;223;1107;677;226;224;102;2;
    223;223;1005;224;539;1001;223;1;223;108;226;677;224;1002;223;2;223;1005;224;554;101;1;223;223;
    1007;226;226;224;102;2;223;223;1005;224;569;101;1;223;223;8;226;226;224;102;2;223;223;1006;224;
    584;101;1;223;223;1008;677;677;224;1002;223;2;223;1005;224;599;1001;223;1;223;107;226;677;224;
    1002;223;2;223;1005;224;614;1001;223;1;223;1108;226;677;224;102;2;223;223;1006;224;629;101;1;
    223;223;7;677;677;224;1002;223;2;223;1005;224;644;1001;223;1;223;108;677;677;224;102;2;223;223;
    1005;224;659;101;1;223;223;1007;677;677;224;102;2;223;223;1006;224;674;101;1;223;223;4;223;99;226
|]

let getinput() = 5

type Mode =
    | POS
    | IMM

type Inst =
    | ADD of (Mode * int) *  (Mode * int) * (Mode * int)
    | MUL of (Mode * int) *  (Mode * int) * (Mode * int)
    | IN of Mode * int * int
    | OUT of Mode * int
    | JIT of (Mode * int) *  (Mode * int)
    | JIF of (Mode * int) *  (Mode * int)
    | LT of (Mode * int) *  (Mode * int) *  (Mode * int)
    | EQ of (Mode * int) *  (Mode * int) *  (Mode * int)
    | HALT

let memSize = function
| ADD _ -> 4
| MUL _ -> 4
| LT _  -> 4
| EQ _  -> 4
| JIT _ -> 3
| JIF _ -> 3
| IN _  -> 2
| OUT _ -> 2
| HALT  -> 1

let digitsRev (x:int) =
    x
    |> string
    |> fun x -> x.ToCharArray()
    |> Array.map (fun x -> int x - int '0')
    |> List.ofArray
    |> List.rev 

let createInst (mem: int[]) pos instModes =
    
    let mode = function | 0 -> POS | _ -> IMM

    let getDigit xs i = xs |> List.tryItem i |> Option.defaultValue 0

    let par3 xs = (mode (getDigit xs 1), mem.[pos+1]),
                  (mode (getDigit xs 2), mem.[pos+2]),
                  (mode (getDigit xs 3), mem.[pos+3])
    
    let par2 xs = (mode (getDigit xs 1), mem.[pos+1]),
                  (mode (getDigit xs 2), mem.[pos+2])

    let par1 xs = (mode (getDigit xs 1), mem.[pos+1])

    match instModes with
    | 1::xs -> ADD (par3 xs)
    | 2::xs -> MUL (par3 xs)
    | 7::xs -> LT (par3 xs)
    | 8::xs -> EQ (par3 xs)
    | 5::xs -> JIT (par2 xs)
    | 6::xs -> JIF (par2 xs)
    | 4::xs -> OUT (par1 xs)
    | 3::xs -> IN (mode (getDigit xs 0), mem.[pos+1], getinput())
    | 9::9::_  -> HALT
    | _ -> failwith "incorrect digit set for createInst"
    
let parseInst (mem: int[]) pos =
    let instModes = mem.[pos] |> digitsRev;
    let inst = createInst mem pos instModes
    (inst, memSize inst)

let getVal (mem: int[]) m i =
    match m with
    | IMM -> i
    | POS -> mem.[i]

type Result =
    | PASS
    | JUMP of int

let runInst (mem: int[]) = function
| ADD ((m1, i1), (m2, i2), (_,i3)) -> 
    mem.[i3] <- (getVal mem m1 i1) + (getVal mem m2 i2)
    PASS
| MUL ((m1, i1), (m2, i2), (_,i3)) -> 
    mem.[i3] <- (getVal mem m1 i1) * (getVal mem m2 i2) 
    PASS
| IN (_, i, v) ->
    mem.[i] <- v
    PASS
| JIT ((m1, i1), (m2, i2)) ->
    match getVal mem m1 i1 with
    | 0 -> PASS
    | _ -> JUMP (getVal mem m2 i2)
| JIF ((m1, i1), (m2, i2)) ->
    match getVal mem m1 i1 with
    | 0 -> JUMP (getVal mem m2 i2)
    | _ -> PASS
| LT ((m1, i1), (m2, i2), (_,i3)) -> 
    if (getVal mem m1 i1) < (getVal mem m2 i2) then
        mem.[i3] <- 1
    else
        mem.[i3] <- 0
    PASS
| EQ ((m1, i1), (m2, i2), (_,i3)) -> 
    if (getVal mem m1 i1) = (getVal mem m2 i2) then
        mem.[i3] <- 1
    else
        mem.[i3] <- 0
    PASS
| OUT (mode, i) ->
    getVal mem mode i |> printfn "%O"
    PASS
| HALT ->
    printfn "HALT";
    PASS

let getPos pos result =
    match result with
    | PASS -> pos
    | JUMP i -> i

let rec runIntCode (mem: int[]) pos =
    let inst, posDiff = parseInst mem pos
    let res = runInst mem inst
    if inst <> HALT then
        runIntCode mem (getPos (pos+posDiff) res)
    else
        ()

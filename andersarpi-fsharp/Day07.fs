module Day07

let program = [|
    3;8;1001;8;10;8;105;1;0;0;21;42;67;84;109;122;203;284;365;446;99999;3;9;1002;9;3;9;1001;9;5;9;102;4;9;9;1001;9;3;9;4;9;99;3;9;1001;9;5;9;1002;9;3;9;1001;9;4;9;102;3;9;9;101;3;9;9;4;9;99;3;9;101;5;9;9;1002;9;3;9;101;5;9;9;4;9;99;3;9;102;5;9;9;101;5;9;9;102;3;9;9;101;3;9;9;102;2;9;9;4;9;99;3;9;101;2;9;9;1002;9;3;9;4;9;99;3;9;101;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;99;3;9;1001;9;1;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;99;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;1;9;4;9;99;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;99
|]

let mutable ioStack = []

let getinput () = 
    match ioStack with
    | [] -> failwith "empty stack"
    | x::xs ->
        ioStack <- xs
        x

let output x =
    ioStack <- x::ioStack

type Mode = POS | IMM

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

type Result = PASS | JUMP of int

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
    getVal mem mode i |> output
    PASS
| HALT ->
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

let runOnce phase input =
    ioStack <- [phase;input]
    runIntCode (Array.copy program) 0
    List.item 0 ioStack

let rec runPhases i ps =
    match ps with
    | [] -> i
    | p::ps -> 
        let o = (runOnce p i)
        runPhases o ps

//stolen from the internet, sorry
let rec ins x = function
| []             -> [[x]]
| (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (ins x ys))

let rec perms = function
| []      -> seq [ [] ]
| x :: xs -> Seq.concat (Seq.map (ins x) (perms xs))

perms [0;1;2;3;4] |> (List.ofSeq >> List.map (runPhases 0)) |> List.max |> printfn "%O"

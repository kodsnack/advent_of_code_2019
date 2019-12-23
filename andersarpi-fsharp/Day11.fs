module Day11

let (++) (x1,y1) (x2,y2) = (x1+x2, y1+y2)
  
//part1
//let mutable tiles = Map.empty

//part2
let mutable tiles = Map.ofList [((0,0), 1)]

let mutable robPos = (0,0)

type Dir = N | S | E | W
let dirToVec = function
| N -> (0,1)
| S -> (0,-1)
| E -> (1,0)
| W -> (-1,0)

let mutable robDir = N
let mutable move = false

let getColor ts p =
    match Map.tryFind p ts with
    | None -> 0
    | Some x -> x

let mutable relPos = 0L

let getinput () = getColor tiles robPos

let output x =
    let x = x |> int
    if move then
        robDir <- match robDir, x with
                  | N, 0 -> W
                  | S, 0 -> E
                  | E, 0 -> N
                  | W, 0 -> S
                  | N, 1 -> E
                  | S, 1 -> W
                  | E, 1 -> S
                  | W, 1 -> N
                  | x,y -> failwithf "incorrect movement args %O %O" x y
        robPos <- robPos ++ (dirToVec robDir)
        move <- false
    else
        tiles <- Map.add robPos x tiles
        move <- true

type Mode = POS | IMM | REL

type Inst =
    | ADD of (Mode * int64) *  (Mode * int64) * (Mode * int64)
    | MUL of (Mode * int64) *  (Mode * int64) * (Mode * int64)
    | IN of Mode * int64 * int64
    | OUT of Mode * int64
    | JIT of (Mode * int64) *  (Mode * int64)
    | JIF of (Mode * int64) *  (Mode * int64)
    | LT of (Mode * int64) *  (Mode * int64) *  (Mode * int64)
    | EQ of (Mode * int64) *  (Mode * int64) *  (Mode * int64)
    | ADJ of Mode * int64
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
| ADJ _ -> 2
| HALT  -> 1

let digitsRev (x: int64) =
    x
    |> string
    |> fun x -> x.ToCharArray()
    |> Array.map (fun x -> int64 x - int64 '0')
    |> List.ofArray
    |> List.rev 

let createInst (mem: int64[]) pos instModes =
    
    let mode = function
    | 0L -> POS
    | 1L -> IMM
    | 2L -> REL
    | x -> failwithf "incorrect mode %O" x

    let getDigit xs i = xs |> List.tryItem i |> Option.defaultValue 0L

    let par3 xs = (mode (getDigit xs 1), mem.[pos+1]),
                  (mode (getDigit xs 2), mem.[pos+2]),
                  (mode (getDigit xs 3), mem.[pos+3])
    
    let par2 xs = (mode (getDigit xs 1), mem.[pos+1]),
                  (mode (getDigit xs 2), mem.[pos+2])

    let par1 xs = (mode (getDigit xs 1), mem.[pos+1])

    match instModes with
    | 9L::9L::_ -> HALT
    | 1L::xs -> ADD (par3 xs)
    | 2L::xs -> MUL (par3 xs)
    | 7L::xs -> LT (par3 xs)
    | 8L::xs -> EQ (par3 xs)
    | 5L::xs -> JIT (par2 xs)
    | 6L::xs -> JIF (par2 xs)
    | 4L::xs -> OUT (par1 xs)
    | 9L::xs -> ADJ (par1 xs)
    | 3L::xs -> IN (mode (getDigit xs 1), mem.[pos+1], getinput() |> int64)
    | x -> failwithf "incorrect digit set for createInst: %O" x
    
let parseInst (mem: int64[]) pos =
    let instModes = mem.[pos] |> digitsRev;
    let inst = createInst mem pos instModes
    (inst, memSize inst)

let getVal (mem: int64[]) m i =
    match m with
    | IMM -> i
    | POS -> mem.[int i]
    | REL -> mem.[relPos + i |> int]

let getPos m i =
    match m with
    | IMM -> failwith "incorrect pos mode"
    | POS -> i
    | REL -> i + relPos

let runInst (mem: int64[]) = function
| ADD ((m1, i1), (m2, i2), (m3,i3)) -> 
    mem.[getPos m3 i3 |> int] <- (getVal mem m1 i1) + (getVal mem m2 i2)
    PASS
| MUL ((m1, i1), (m2, i2), (m3,i3)) -> 
    mem.[getPos m3 i3 |> int] <- (getVal mem m1 i1) * (getVal mem m2 i2) 
    PASS
| IN (m1, i1, v) ->
    mem.[getPos m1 i1 |> int] <- v
    PASS
| JIT ((m1, i1), (m2, i2)) ->
    match getVal mem m1 i1 with
    | 0L -> PASS
    | _ -> JUMP (getVal mem m2 i2 |> int)
| JIF ((m1, i1), (m2, i2)) ->
    match getVal mem m1 i1 with
    | 0L -> JUMP (getVal mem m2 i2 |> int)
    | _ -> PASS
| LT ((m1, i1), (m2, i2), (m3,i3)) -> 
    if (getVal mem m1 i1) < (getVal mem m2 i2) then
        mem.[getPos m3 i3 |> int] <- 1L
    else
        mem.[getPos m3 i3 |> int] <- 0L
    PASS
| EQ ((m1, i1), (m2, i2), (m3,i3)) -> 
    if (getVal mem m1 i1) = (getVal mem m2 i2) then
        mem.[getPos m3 i3 |> int] <- 1L
    else
        mem.[getPos m3 i3 |> int] <- 0L
    PASS
| OUT (mode, i) ->
    getVal mem mode i |> output
    PASS
| ADJ (mode, i) ->
    relPos <- relPos + (getVal mem mode i)
    PASS
| HALT ->
    PASS

let getNextPos pos result =
    match result with
    | PASS -> pos
    | JUMP i -> i

let runIntCode (mem: int64[]) =
    let rec run mem' pos =
        let inst, posDiff = parseInst mem' pos
        let res = runInst mem' inst
        if inst <> HALT then
            run mem' (getNextPos (pos+posDiff) res)
        else
            ()
    run mem 0

let programCode = [|
    3L;8L;1005L;8L;326L;1106L;0L;11L;0L;0L;0L;104L;1L;104L;0L;3L;8L;102L;-1L;8L;10L;101L;1L;10L;10L;4L;10L;1008L;8L;1L;10L;4L;10L;1001L;8L;0L;29L;2L;1003L;17L;10L;1006L;0L;22L;2L;106L;5L;10L;1006L;0L;87L;3L;8L;102L;-1L;8L;10L;101L;1L;10L;10L;4L;10L;1008L;8L;1L;10L;4L;10L;1001L;8L;0L;65L;2L;7L;20L;10L;2L;9L;17L;10L;2L;6L;16L;10L;3L;8L;102L;-1L;8L;10L;1001L;10L;1L;10L;4L;10L;1008L;8L;0L;10L;4L;10L;101L;0L;8L;99L;1006L;0L;69L;1006L;0L;40L;3L;8L;102L;-1L;8L;10L;1001L;10L;1L;10L;4L;10L;1008L;8L;1L;10L;4L;10L;101L;0L;8L;127L;1006L;0L;51L;2L;102L;17L;10L;3L;8L;1002L;8L;-1L;10L;1001L;10L;1L;10L;4L;10L;108L;1L;8L;10L;4L;10L;1002L;8L;1L;155L;1006L;0L;42L;3L;8L;1002L;8L;-1L;10L;101L;1L;10L;10L;4L;10L;108L;0L;8L;10L;4L;10L;101L;0L;8L;180L;1L;106L;4L;10L;2L;1103L;0L;10L;1006L;0L;14L;3L;8L;102L;-1L;8L;10L;1001L;10L;1L;10L;4L;10L;108L;0L;8L;10L;4L;10L;1001L;8L;0L;213L;1L;1009L;0L;10L;3L;8L;1002L;8L;-1L;10L;1001L;10L;1L;10L;4L;10L;108L;0L;8L;10L;4L;10L;1002L;8L;1L;239L;1006L;0L;5L;2L;108L;5L;10L;2L;1104L;7L;10L;3L;8L;102L;-1L;8L;10L;101L;1L;10L;10L;4L;10L;108L;0L;8L;10L;4L;10L;102L;1L;8L;272L;2L;1104L;12L;10L;1L;1109L;10L;10L;3L;8L;102L;-1L;8L;10L;1001L;10L;1L;10L;4L;10L;108L;1L;8L;10L;4L;10L;102L;1L;8L;302L;1006L;0L;35L;101L;1L;9L;9L;1007L;9L;1095L;10L;1005L;10L;15L;99L;109L;648L;104L;0L;104L;1L;21102L;937268449940L;1L;1L;21102L;1L;343L;0L;1105L;1L;447L;21101L;387365315480L;0L;1L;21102L;1L;354L;0L;1105L;1L;447L;3L;10L;104L;0L;104L;1L;3L;10L;104L;0L;104L;0L;3L;10L;104L;0L;104L;1L;3L;10L;104L;0L;104L;1L;3L;10L;104L;0L;104L;0L;3L;10L;104L;0L;104L;1L;21101L;0L;29220891795L;1L;21102L;1L;401L;0L;1106L;0L;447L;21101L;0L;248075283623L;1L;21102L;412L;1L;0L;1105L;1L;447L;3L;10L;104L;0L;104L;0L;3L;10L;104L;0L;104L;0L;21101L;0L;984353760012L;1L;21102L;1L;435L;0L;1105L;1L;447L;21102L;1L;718078227200L;1L;21102L;1L;446L;0L;1105L;1L;447L;99L;109L;2L;21202L;-1L;1L;1L;21102L;40L;1L;2L;21101L;0L;478L;3L;21101L;468L;0L;0L;1106L;0L;511L;109L;-2L;2106L;0L;0L;0L;1L;0L;0L;1L;109L;2L;3L;10L;204L;-1L;1001L;473L;474L;489L;4L;0L;1001L;473L;1L;473L;108L;4L;473L;10L;1006L;10L;505L;1102L;1L;0L;473L;109L;-2L;2105L;1L;0L;0L;109L;4L;1202L;-1L;1L;510L;1207L;-3L;0L;10L;1006L;10L;528L;21102L;1L;0L;-3L;22102L;1L;-3L;1L;22101L;0L;-2L;2L;21101L;0L;1L;3L;21102L;1L;547L;0L;1105L;1L;552L;109L;-4L;2105L;1L;0L;109L;5L;1207L;-3L;1L;10L;1006L;10L;575L;2207L;-4L;-2L;10L;1006L;10L;575L;21202L;-4L;1L;-4L;1105L;1L;643L;21202L;-4L;1L;1L;21201L;-3L;-1L;2L;21202L;-2L;2L;3L;21102L;1L;594L;0L;1106L;0L;552L;22102L;1L;1L;-4L;21101L;1L;0L;-1L;2207L;-4L;-2L;10L;1006L;10L;613L;21101L;0L;0L;-1L;22202L;-2L;-1L;-2L;2107L;0L;-3L;10L;1006L;10L;635L;22101L;0L;-1L;1L;21101L;0L;635L;0L;106L;0L;510L;21202L;-2L;-1L;-2L;22201L;-4L;-2L;-4L;109L;-5L;2105L;1L;0L
|]

let program : int64 [] = Array.create 10000 0L
programCode.CopyTo(program, 0)

let run() =
    Array.copy program |> runIntCode
    tiles |> Map.toSeq |> Seq.length |> printfn "%O"

    for y in -50..50 do
        for x in -50..50 do
            match Map.tryFind (x,y) tiles with
            | Some 1 -> printf "#" 
            | _      -> printf " "
        printfn ""

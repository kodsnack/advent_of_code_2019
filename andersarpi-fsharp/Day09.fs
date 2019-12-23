module Day09

let mutable relPos = 0L

let mutable ioStack = []

let getinput () = 
    match ioStack with
    | [] -> failwith "empty stack"
    | x::xs ->
        ioStack <- xs
        x

let output x =
    printfn "%O" x

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
    | 3L::xs -> IN (mode (getDigit xs 1), mem.[pos+1], getinput())
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
    1102L;34463338L;34463338L;63L;1007L;63L;34463338L;63L;1005L;63L;53L;1101L;3L;0L;1000L;109L;988L;209L;12L;9L;1000L;209L;6L;209L;3L;203L;0L;1008L;1000L;1L;63L;1005L;63L;65L;1008L;1000L;2L;63L;1005L;63L;904L;1008L;1000L;0L;63L;1005L;63L;58L;4L;25L;104L;0L;99L;4L;0L;104L;0L;99L;4L;17L;104L;0L;99L;0L;0L;1101L;25L;0L;1016L;1102L;760L;1L;1023L;1102L;1L;20L;1003L;1102L;1L;22L;1015L;1102L;1L;34L;1000L;1101L;0L;32L;1006L;1101L;21L;0L;1017L;1102L;39L;1L;1010L;1101L;30L;0L;1005L;1101L;0L;1L;1021L;1101L;0L;0L;1020L;1102L;1L;35L;1007L;1102L;1L;23L;1014L;1102L;1L;29L;1019L;1101L;767L;0L;1022L;1102L;216L;1L;1025L;1102L;38L;1L;1011L;1101L;778L;0L;1029L;1102L;1L;31L;1009L;1101L;0L;28L;1004L;1101L;33L;0L;1008L;1102L;1L;444L;1027L;1102L;221L;1L;1024L;1102L;1L;451L;1026L;1101L;787L;0L;1028L;1101L;27L;0L;1018L;1101L;0L;24L;1013L;1102L;26L;1L;1012L;1101L;0L;36L;1002L;1102L;37L;1L;1001L;109L;28L;21101L;40L;0L;-9L;1008L;1019L;41L;63L;1005L;63L;205L;1001L;64L;1L;64L;1105L;1L;207L;4L;187L;1002L;64L;2L;64L;109L;-9L;2105L;1L;5L;4L;213L;1106L;0L;225L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-9L;1206L;10L;243L;4L;231L;1001L;64L;1L;64L;1105L;1L;243L;1002L;64L;2L;64L;109L;-3L;1208L;2L;31L;63L;1005L;63L;261L;4L;249L;1106L;0L;265L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;5L;21108L;41L;41L;0L;1005L;1012L;287L;4L;271L;1001L;64L;1L;64L;1105L;1L;287L;1002L;64L;2L;64L;109L;6L;21102L;42L;1L;-5L;1008L;1013L;45L;63L;1005L;63L;307L;1105L;1L;313L;4L;293L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-9L;1201L;0L;0L;63L;1008L;63L;29L;63L;1005L;63L;333L;1106L;0L;339L;4L;319L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-13L;2102L;1L;4L;63L;1008L;63L;34L;63L;1005L;63L;361L;4L;345L;1105L;1L;365L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;5L;1201L;7L;0L;63L;1008L;63L;33L;63L;1005L;63L;387L;4L;371L;1105L;1L;391L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;7L;1202L;1L;1L;63L;1008L;63L;32L;63L;1005L;63L;411L;1105L;1L;417L;4L;397L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;20L;1205L;-7L;431L;4L;423L;1106L;0L;435L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;2L;2106L;0L;-3L;1001L;64L;1L;64L;1105L;1L;453L;4L;441L;1002L;64L;2L;64L;109L;-7L;21101L;43L;0L;-9L;1008L;1014L;43L;63L;1005L;63L;479L;4L;459L;1001L;64L;1L;64L;1105L;1L;479L;1002L;64L;2L;64L;109L;-5L;21108L;44L;43L;0L;1005L;1018L;495L;1105L;1L;501L;4L;485L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-7L;1205L;9L;517L;1001L;64L;1L;64L;1105L;1L;519L;4L;507L;1002L;64L;2L;64L;109L;11L;1206L;-1L;531L;1106L;0L;537L;4L;525L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-15L;1208L;0L;36L;63L;1005L;63L;557L;1001L;64L;1L;64L;1106L;0L;559L;4L;543L;1002L;64L;2L;64L;109L;7L;2101L;0L;-7L;63L;1008L;63L;35L;63L;1005L;63L;581L;4L;565L;1106L;0L;585L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-3L;21107L;45L;46L;4L;1005L;1015L;607L;4L;591L;1001L;64L;1L;64L;1105L;1L;607L;1002L;64L;2L;64L;109L;-16L;2102L;1L;10L;63L;1008L;63L;31L;63L;1005L;63L;631L;1001L;64L;1L;64L;1106L;0L;633L;4L;613L;1002L;64L;2L;64L;109L;1L;2107L;33L;10L;63L;1005L;63L;649L;1106L;0L;655L;4L;639L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;17L;2101L;0L;-9L;63L;1008L;63L;31L;63L;1005L;63L;679L;1001L;64L;1L;64L;1106L;0L;681L;4L;661L;1002L;64L;2L;64L;109L;-6L;2107L;34L;0L;63L;1005L;63L;703L;4L;687L;1001L;64L;1L;64L;1106L;0L;703L;1002L;64L;2L;64L;109L;5L;1207L;-5L;34L;63L;1005L;63L;719L;1105L;1L;725L;4L;709L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-15L;1202L;6L;1L;63L;1008L;63L;20L;63L;1005L;63L;751L;4L;731L;1001L;64L;1L;64L;1105L;1L;751L;1002L;64L;2L;64L;109L;21L;2105L;1L;5L;1001L;64L;1L;64L;1106L;0L;769L;4L;757L;1002L;64L;2L;64L;109L;5L;2106L;0L;5L;4L;775L;1001L;64L;1L;64L;1106L;0L;787L;1002L;64L;2L;64L;109L;-27L;1207L;4L;35L;63L;1005L;63L;809L;4L;793L;1001L;64L;1L;64L;1106L;0L;809L;1002L;64L;2L;64L;109L;13L;2108L;33L;-1L;63L;1005L;63L;831L;4L;815L;1001L;64L;1L;64L;1106L;0L;831L;1002L;64L;2L;64L;109L;4L;21107L;46L;45L;1L;1005L;1014L;851L;1001L;64L;1L;64L;1105L;1L;853L;4L;837L;1002L;64L;2L;64L;109L;3L;21102L;47L;1L;-3L;1008L;1013L;47L;63L;1005L;63L;875L;4L;859L;1106L;0L;879L;1001L;64L;1L;64L;1002L;64L;2L;64L;109L;-9L;2108L;28L;2L;63L;1005L;63L;895L;1106L;0L;901L;4L;885L;1001L;64L;1L;64L;4L;64L;99L;21101L;27L;0L;1L;21102L;1L;915L;0L;1106L;0L;922L;21201L;1L;59074L;1L;204L;1L;99L;109L;3L;1207L;-2L;3L;63L;1005L;63L;964L;21201L;-2L;-1L;1L;21102L;942L;1L;0L;1105L;1L;922L;21201L;1L;0L;-1L;21201L;-2L;-3L;1L;21102L;1L;957L;0L;1105L;1L;922L;22201L;1L;-1L;-2L;1106L;0L;968L;22102L;1L;-2L;-2L;109L;-3L;2105L;1L;0L
|]

let program : int64 [] = Array.create 10000 0L
programCode.CopyTo(program, 0)

let part1() =
    ioStack <- [1L]
    Array.copy program |> runIntCode

let part2() =
    ioStack <- [2L]
    Array.copy program |> runIntCode
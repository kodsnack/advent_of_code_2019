import strutils
import math
import logging
import streams

const
    HaltInstCode = 99

type
    Address* = int
    Memory* = ref object
        storage: seq[Address]
    Instruction = proc (mem: var Memory, pc: var Address): bool
    InOut = object
        input: Stream
        output: Stream

var
    logger = newConsoleLogger(lvlWarn)
    io: InOut

proc `[]`*(mem: Memory, idx: Address): int =
    result = mem.storage[idx]

proc `[]=`*(mem: var Memory, idx: Address, value: int) =
    mem.storage[idx] = value
    
proc fetchArgument(mem: Memory, pc: Address, argNo: int): int =
    let
        mode = (mem[pc] div 10^(argNo + 1)) mod 10
    if mode == 1:
        return mem[pc+argNo]
    else:
        return mem[mem[pc+argNo]]

proc doAdd(mem: var Memory, pc: var Address): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
        target = mem[pc + 3]
    logger.log(lvlDebug, $pc & " ADD " & $value1 & " + " & $value2 & " => " & $target)
    mem[target] = value1 + value2
    pc.inc(4)
    return true

proc doMul(mem: var Memory, pc: var Address): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
        target = mem[pc + 3]
    logger.log(lvlDebug, $pc & " MUL " & $value1 & " + " & $value2 & " => " & $target)
    mem[target] = value1 * value2
    pc.inc(4)
    return true

proc doInp(mem: var Memory, pc: var Address): bool =
    let
        target = mem[pc + 1]
    logger.log(lvlDebug, $pc & " INP => " & $target)
    mem[target] = io.input.readLine.parseInt
    pc.inc(2)
    return true

proc doOut(mem: var Memory, pc: var Address): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
    logger.log(lvlDebug, $pc & " OUT " & $value1 & " => ")
    io.output.writeLine($value1)
    pc.inc(2)        
    return true

proc doJit(mem: var Memory, pc: var Address): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
    logger.log(lvlDebug, $pc & " JIT " & $value1 & " ? " & $value2 & " => PC")
    if value1 != 0:
        pc = value2
    else:
        pc.inc(3)        
    return true

proc doJif(mem: var Memory, pc: var Address): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
    logger.log(lvlDebug, $pc & " JIF " & $value1 & " ? " & $value2 & " => PC")
    if value1 == 0:
        pc = value2
    else:
        pc.inc(3)        
    return true
    
proc doLt(mem: var Memory, pc: var Address): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
        target = mem[pc + 3]
    logger.log(lvlDebug, $pc & " LT " & $value1 & " < " & $value2 & " => " & $target)
    if value1 < value2:
        mem[target] = 1
    else:
        mem[target] = 0
    pc.inc(4)
    return true

proc doEq(mem: var Memory, pc: var Address): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
        target = mem[pc + 3]
    logger.log(lvlDebug, $pc & " EQ " & $value1 & " == " & $value2 & " => " & $target)
    if value1 == value2:
        mem[target] = 1
    else:
        mem[target] = 0
    pc.inc(4)
    return true
    
proc fetchInstruction(mem: Memory, pc: Address): Instruction =
    const
        instList = [nil, doAdd, doMul, doInp, doOut, doJit, doJif,
                    doLt, doEq]
    let
        instCode = mem[pc] mod 100
    if instCode >= instList.len or instCode <= 0:
        if instCode != HaltInstCode:
            logger.log(lvlWarn, "Invalid instruction ", instCode, " at address ", pc)
        return nil
    return instList[instCode]

proc loadMemory*(inp: string): Memory =
    new(result)
    result.storage = newSeqOfCap[int](inp.len div 2)
    for valueString in inp.split(','):
        let
            value = valueString.parseInt()
        result.storage.add(value)

proc InstructionDispatcher(mem: var Memory, pc: var Address): bool =
    result = true
    let
        instruction = fetchInstruction(mem, pc)
    if instruction != nil:
        result = instruction(mem, pc)
    else:
        result = false
    
proc run*(mem: var Memory, interactive=true, userInput=""): string =
    var
        pc = 0
    if interactive:
        io.input = newFileStream(stdin)
        io.output = newFileStream(stdout)
    else:
        io.input = newStringStream(userInput)
        io.output = newStringStream("")
    while InstructionDispatcher(mem, pc):
        discard
    if not interactive:
        io.output.setPosition(0)
        result = io.output.readAll()
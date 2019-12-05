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
    Instruction = proc (mem: var Memory, pc: var Address, input: string, output: var string): bool
    InOut = object
        inpup: Stream
        output: Stream

var
    logger = newConsoleLogger()

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

proc doAdd(mem: var Memory, pc: var Address, input: string, output: var string): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
        target = mem[pc + 3]
    logger.log(lvlDebug, $pc & " ADD " & $value1 & " + " & $value2 & " => " & $target)
    mem[target] = value1 + value2
    pc.inc(4)

proc doMul(mem: var Memory, pc: var Address, input: string, output: var string): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
        target = mem[pc + 3]
    logger.log(lvlDebug, $pc & " MUL " & $value1 & " + " & $value2 & " => " & $target)
    mem[target] = value1 * value2
    pc.inc(4)

proc doMul(mem: var Memory, pc: var Address, input: string, output: var string): bool =
    let
        value1 = fetchArgument(mem, pc, 1)
        value2 = fetchArgument(mem, pc, 2)
        target = mem[pc + 3]
    logger.log(lvlDebug, $pc & " MUL " & $value1 & " + " & $value2 & " => " & $target)
    mem[target] = value1 * value2
    pc.inc(4)    

proc fetchInstruction(mem: Memory, pc: Address): Instruction =
    const
        instList = [nil, doAdd, doMul]
    let
        instCode = mem[pc] mod 100
    if instCode >= instList.len or instCode <= 0:
        if instCode != HaltInstCode:
            echo("Invalid instruction ", instCode, " at address ", pc)
        return nil
    return instList[instCode]

proc loadMemory*(inp: string): Memory =
    new(result)
    result.storage = newSeqOfCap[int](inp.len div 2)
    for valueString in inp.split(','):
        let
            value = valueString.parseInt()
        result.storage.add(value)

proc InstructionDispatcher(mem: var Memory, pc: var Address, input: string, output: var string): bool =
    result = true
    let
        instruction = fetchInstruction(mem, pc)
    if instruction != nil:
        result = instruction(mem, pc, input, output)
    else:
        result = false
    
proc run*(mem: var Memory, userInput=""): string =
    var
        pc = 0
    while InstructionDispatcher(mem, pc, userInput, result):
        discard
    

import strutils
import math
import logging
import deques
import tables
import algorithm

type
    Address* = int
    Memory* = ref object
        storage: seq[Address]
    Instruction = proc (cpu: var Cpu)
    InOut = object
        input: Deque[int]
        output: Deque[int]
    ProgramState = enum
        PSReady, PSWaiting, PSHalted
    Cpu* = ref object
        mem: Memory
        pc: Address
        io: InOut
        state: ProgramState

var
    logger = newConsoleLogger(lvlWarn)

proc `[]`(mem: Memory, idx: Address): int =
    result = mem.storage[idx]

proc `[]=`(mem: var Memory, idx: Address, value: int) =
    mem.storage[idx] = value
    
proc fetchArgument(cpu: Cpu, argNo: int): int =
    let
        mode = (cpu.mem[cpu.pc] div 10^(argNo + 1)) mod 10
    if mode == 1:
        return cpu.mem[cpu.pc+argNo]
    else:
        return cpu.mem[cpu.mem[cpu.pc+argNo]]

proc doAdd(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
        target = cpu.mem[cpu.pc + 3]
    logger.log(lvlDebug, $cpu.pc & " ADD " & $value1 & " + " & $value2 & " => " & $target)
    cpu.mem[target] = value1 + value2
    cpu.pc.inc(4)

proc doMul(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
        target = cpu.mem[cpu.pc + 3]
    logger.log(lvlDebug, $cpu.pc & " MUL " & $value1 & " * " & $value2 & " => " & $target)
    cpu.mem[target] = value1 * value2
    cpu.pc.inc(4)

proc doInp(cpu: var Cpu) =
    if cpu.io.input.len == 0:
        logger.log(lvlDebug, $cpu.pc & " INP => (waiting) ")
        cpu.state = PSWaiting
        return
    let
        target = cpu.mem[cpu.pc + 1]
        val = cpu.io.input.popFirst
    logger.log(lvlDebug, cpu.pc, " INP ", val, "=> ", target)
    cpu.mem[target] = val
    cpu.pc.inc(2)

proc doOut(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
    logger.log(lvlDebug, $cpu.pc & " OUT " & $value1 & " => ")
    cpu.io.output.addLast(value1)
    cpu.pc.inc(2)

proc doJit(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
    logger.log(lvlDebug, $cpu.pc & " JIT " & $value1 & " ? " & $value2 & " => PC")
    if value1 != 0:
        cpu.pc = value2
    else:
        cpu.pc.inc(3)        

proc doJif(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
    logger.log(lvlDebug, $cpu.pc & " JIF " & $value1 & " ? " & $value2 & " => PC")
    if value1 == 0:
        cpu.pc = value2
    else:
        cpu.pc.inc(3)

proc doLt(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
        target = cpu.mem[cpu.pc + 3]
    logger.log(lvlDebug, $cpu.pc & " LT " & $value1 & " < " & $value2 & " => " & $target)
    if value1 < value2:
        cpu.mem[target] = 1
    else:
        cpu.mem[target] = 0
    cpu.pc.inc(4)

proc doEq(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
        target = cpu.mem[cpu.pc + 3]
    logger.log(lvlDebug, $cpu.pc & " EQ " & $value1 & " == " & $value2 & " => " & $target)
    if value1 == value2:
        cpu.mem[target] = 1
    else:
        cpu.mem[target] = 0
    cpu.pc.inc(4)

proc doHalt(cpu: var Cpu) =
    logger.log(lvlDebug, $cpu.pc & " HALT ")
    cpu.state = PSHalted
    
proc fetchInstruction(cpu: Cpu): Instruction =
    const
        instList = {1: doAdd, 2: doMul, 3: doInp, 4: doOut, 5: doJit, 6: doJif,
                    7:doLt, 8:doEq, 99:doHalt}.toTable
    let
        instCode = cpu.mem[cpu.pc] mod 100
    if not instList.contains(instCode):
        logger.log(lvlWarn, "Error, instruction fetch failed ", instCode, " ", cpu.pc)
        return nil
    return instList[instCode]

proc loadMemory*(inp: string): Memory =
    new(result)
    result.storage = newSeqOfCap[int](inp.len div 2)
    for valueString in inp.split(','):
        let
            value = valueString.parseInt()
        result.storage.add(value)

proc InstructionDispatcher(cpu: var Cpu) =
    let
        instruction = fetchInstruction(cpu)
    if instruction != nil:
        instruction(cpu)

proc step(cpu: var Cpu) =
    if cpu.state != PSHalted:
        cpu.InstructionDispatcher

proc newCpu(prog: string): Cpu =
    new(result)
    result.mem = loadMemory(prog)
    result.state = PSReady
    result.pc = 0
    result.io.input = initDeque[int]()
    result.io.output = initDeque[int]()

proc inp(cpu: var Cpu, val: int) =
    cpu.io.input.addLast(val)
    if cpu.state == PSWaiting:
        cpu.state = PSReady

proc outpIsEmpty(cpu: Cpu): bool =
    return cpu.io.output.len == 0

proc outp(cpu: var Cpu): int =
    result = cpu.io.output.popFirst 

proc createThruster(prog: string, setting: int): Cpu =
    result = newCpu(prog)
    result.inp(setting)

proc runThrusters*(prog: string, settings: seq[int]): int =
    var
        thrusters = newSeqOfCap[Cpu](5)
    for setting in settings:
        thrusters.add(createThruster(prog, setting))
    thrusters[0].inp(0)
    while true:
        var
            noHalted = 0
        for idx, thruster in thrusters.mpairs:
            if thruster.state == PSHalted:
                noHalted.inc
                continue
            var
                prevThruster = thrusters[(idx + len(thrusters) - 1) mod len(thrusters)]
            if not prevThruster.outpIsEmpty:
                thruster.inp(prevThruster.outp)
            thruster.step()
        if noHalted == len(thrusters):
            break
    result = thrusters[len(thrusters)-1].outp

proc optimiseThrusters*(prog: string): int =
    var
        settings = @[0, 1, 2, 3, 4]
    while true:
        result = max(result, runThrusters(prog, settings))
        if not settings.nextPermutation():
            break

proc optimiseThrustersFeedback*(prog: string): int =
    var
        settings = @[5, 6, 7, 8, 9]
    while true:
        result = max(result, runThrusters(prog, settings))
        if not settings.nextPermutation():
            break
            
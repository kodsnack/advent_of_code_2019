import strutils
import math
import logging
import deques
import tables

type
    Address* = int64
    Value* = int64
    Memory* = ref object
        storage: Table[Address, Value]
    Instruction = proc (cpu: var Cpu)
    InOut = object
        input: Deque[Value]
        output: Deque[Value]
    ProgramState = enum
        PSReady, PSWaiting, PSHalted
    ErrorLogEntry = tuple[address:Address, msg: string]
    ErrorLog = seq[ErrorLogEntry]
    Cpu* = ref object
        mem: Memory
        pc: Address
        io: InOut
        state: ProgramState
        addressBase: Address
        errorLog: ErrorLog
    TrapError = object of CatchableError

var
    # logger = newConsoleLogger()
    logger = newConsoleLogger(lvlWarn)

proc `[]`(mem: Memory, idx: Address): Value =
    result = mem.storage.getOrDefault(idx, 0)

proc `[]=`(mem: var Memory, idx: Address, value: Value) =
    mem.storage[idx] = value

proc trap(cpu: Cpu, msg: string) =
    cpu.errorLog.add((address: cpu.pc, msg: msg))
    cpu.state = PSHalted
    raise new(TrapError)

proc fetchArgument(cpu: Cpu, argNo: int): Value =
    let
        mode = (cpu.mem[cpu.pc] div 10^(argNo + 1)) mod 10
    if mode == 0:
        return cpu.mem[cpu.mem[cpu.pc+argNo]]
    if mode == 1:
        return cpu.mem[cpu.pc+argNo]
    elif mode == 2:
        return cpu.mem[cpu.mem[cpu.pc+argNo] + cpu.addressBase]
    else:
        cpu.trap("Illegal read addressing mode " & $mode)

proc fetchTargetAddress(cpu: Cpu, argNo: int): Address =
    let
        mode = (cpu.mem[cpu.pc] div 10^(argNo + 1)) mod 10
    if mode == 0:
        return cpu.mem[cpu.pc+argNo]
    elif mode == 2:
        return cpu.mem[cpu.pc+argNo] + cpu.addressBase
    else:
        cpu.trap("Illegal write addressing mode " & $mode)

proc doAdd(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
        target = cpu.fetchTargetAddress(3)
    logger.log(lvlDebug, $cpu.pc & " ADD " & $value1 & " + " & $value2 & " => " & $target)
    cpu.mem[target] = value1 + value2
    cpu.pc.inc(4)

proc doMul(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
        value2 = cpu.fetchArgument(2)
        target = cpu.fetchTargetAddress(3)
    logger.log(lvlDebug, $cpu.pc & " MUL " & $value1 & " * " & $value2 & " => " & $target)
    cpu.mem[target] = value1 * value2
    cpu.pc.inc(4)

proc doInp(cpu: var Cpu) =
    if cpu.io.input.len == 0:
        logger.log(lvlDebug, $cpu.pc & " INP => (waiting) ")
        cpu.state = PSWaiting
        return
    let
        target = cpu.fetchTargetAddress(1)
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
        target = cpu.fetchTargetAddress(3)
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
        target = cpu.fetchTargetAddress(3)
    logger.log(lvlDebug, $cpu.pc & " EQ " & $value1 & " == " & $value2 & " => " & $target)
    if value1 == value2:
        cpu.mem[target] = 1
    else:
        cpu.mem[target] = 0
    cpu.pc.inc(4)

proc doSetBase(cpu: var Cpu) =
    let
        value1 = cpu.fetchArgument(1)
    logger.log(lvlDebug, $cpu.pc & " BAS " & $value1 & " => BASE" )
    cpu.addressBase = cpu.addressBase + value1
    cpu.pc.inc(2)

proc doHalt(cpu: var Cpu) =
    logger.log(lvlDebug, $cpu.pc & " HALT ")
    cpu.state = PSHalted
    
proc fetchInstruction(cpu: Cpu): Instruction =
    const
        instList = {1: doAdd, 2: doMul, 3: doInp,
                    4: doOut, 5: doJit, 6: doJif,
                    7:doLt, 8:doEq, 9:doSetBase,
                    99:doHalt}.toTable
    let
        instCode:int32 = int32(cpu.mem[cpu.pc] mod 100'i64)
    if not instList.contains(instCode):
        cpu.trap("Error, intruction fetch failed " & $instCode)
        return nil
    return instList[instCode]

proc loadMemory*(inp: string): Memory =
    new(result)
    var
        address: Address = 0
    for valueString in inp.split(','):
        let
            value = valueString.parseBiggestInt()
        result.storage.add(address, value)
        address.inc

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
    result.io.input = initDeque[Value]()
    result.io.output = initDeque[Value]()

proc inp(cpu: var Cpu, val: int64) =
    cpu.io.input.addLast(val)
    if cpu.state == PSWaiting:
        cpu.state = PSReady

proc outpIsEmpty(cpu: Cpu): bool =
    return cpu.io.output.len == 0

proc outp(cpu: var Cpu): Value =
    result = cpu.io.output.popFirst 

proc runProgram*(prog: string, input: seq[Value]): seq[Value] =
    result = newSeqOfCap[Value](16)
    var
        cpu =  newCpu(prog)
    for inVal in input:
        cpu.inp(inVal)
    try:
        while cpu.state == PSReady:
            cpu.step
    except TrapError:
        if cpu.errorLog.len > 0:
            for error in cpu.errorLog:
                echo(error.address, ":", error.msg)
    while not cpu.outpIsEmpty():
        result.add(cpu.outp)

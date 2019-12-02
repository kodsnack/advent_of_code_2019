import strutils

const
    HaltInstCode = 99
    AddInstCode = 1
    MulInstCode = 2

type
    Address* = int
    Memory* = ref object
        storage: seq[Address]

const
    NounPosition: Address = 1
    VerbPosition: Address = 2

proc loadMemory*(inp: string): Memory =
    new(result)
    result.storage = newSeqOfCap[int](inp.len div 2)
    for valueString in inp.split(','):
        let
            value = valueString.parseInt()
        result.storage.add(value)

proc `[]`*(mem: Memory, idx: Address): int =
    result = mem.storage[idx]

proc `[]=`*(mem: var Memory, idx: Address, value: int) =
    mem.storage[idx] = value

proc doAdd(mem: var Memory, pc: var Address) =
    let
        source1 = mem[pc + 1]
        value1 = mem[source1]
        source2 = mem[pc + 2]
        value2 = mem[source2]
        target = mem[pc + 3]
    mem[target] = value1 + value2
    pc.inc(4)

proc doMul(mem: var Memory, pc: var Address) =
    let
        source1 = mem[pc + 1]
        value1 = mem[source1]
        source2 = mem[pc + 2]
        value2 = mem[source2]
        target = mem[pc + 3]
    mem[target] = value1 * value2
    pc.inc(4)

proc InstructionDispatcher(mem: var Memory, pc: var Address): bool =
    result = true
    let
        instructionCode = mem[pc]
    case instructionCode
    of AddInstCode:
        doAdd(mem, pc)
    of MulInstCode:
        doMul(mem, pc)
    of HaltInstCode:
        result = false
    else:
        echo("Invalid instruction ", instructionCode, " at address ", pc)
        result = false
    
proc run*(mem: var Memory) =
    var
        pc = 0
    while InstructionDispatcher(mem, pc):
        discard
    
proc setNoun*(mem: var Memory, noun: int) =
    mem[NounPosition] = noun

proc setVerb*(mem: var Memory, verb: int) =
    mem[VerbPosition] = verb

proc findNounAndVerb*(input: string, key: int): int =
    for noun in 0..99:
        for verb in 0..99:
            var
                mem = loadMemory(input)
            mem.setNoun(noun)
            mem.setVerb(verb)
            mem.run()
            if mem[0] == key:
                return 100 * noun + verb

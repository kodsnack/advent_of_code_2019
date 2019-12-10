from enum import Enum

class status(Enum):
    HALT = 1
    GET_INPUT = 2

class Memory(list):
    def keycheck(self, key):
        if self.__len__() <= key:
            diff = key - self.__len__()
            while diff >= 0:
                self.append(0)
                diff -= 1

    def __getitem__(self, key):
        self.keycheck(key)
        return list.__getitem__(self, key)

    def __setitem__(self, key, value):
        self.keycheck(key)
        return list.__setitem__(self, key, value)


def getInitialMemory():
    f = open("9dec-input", 'r')
    file_content = f.read()
    f.close()
    memory = Memory(map(int, file_content.split(',')))
    return memory

def getOpcode(inst):
    inst_str = str(inst)
    if len(inst_str) < 2:
        inst_str = "0" + inst_str
    opcode = int(inst_str[-2:])
    return opcode

def getArgs(inst, num, memory, pc, rc):
    inst_str = str(inst)
    if len(inst_str) < 2:
        inst_str = "0" + inst_str

    modes = list(inst_str[:-2])
    modes.reverse()

    args = []
    for i in range(num):
        if len(modes) <= i:
            modes += "0"
        mode = int(modes[i])

        arg = 0
        if (mode == 0):
            addr = memory[pc]
        elif (mode == 1):
            addr = pc
        elif (mode == 2):
            addr = memory[pc] + rc
        else:
            print("Unknown mode: %d at %d" % (mode, pc))
            exit()
        args.append(addr)
        pc += 1
    return args

def run(memory, inputs, pc = 0, rc = 0):
    outputs = []

    while(True):
        instr = memory[pc]
        opcode = getOpcode(instr)
        pc += 1
        result = 0
        addr = -1

        if (opcode == 99):
            break
        elif (opcode == 1):
            args = getArgs(instr, 3, memory, pc, rc)
            arg1 = memory[args[0]]
            arg2 = memory[args[1]]
            addr = args[2]
            pc += 3
            result = arg1 + arg2
        elif (opcode == 2):
            args = getArgs(instr, 3, memory, pc, rc)
            arg1 = memory[args[0]]
            arg2 = memory[args[1]]
            addr = args[2]
            pc += 3
            result = arg1 * arg2
        elif (opcode == 3):
            if len(inputs) == 0:
                return [status.GET_INPUT, pc - 1, outputs]
            result = int(inputs.pop(0))
            args = getArgs(instr, 1, memory, pc, rc)
            addr = args[0]
            pc += 1
        elif (opcode == 4):
            args = getArgs(instr, 1, memory, pc, rc)
            arg1 = memory[args[0]]
            pc += 1
            outputs.append(arg1)
        elif (opcode == 5):
            args = getArgs(instr, 2, memory, pc, rc)
            arg1 = memory[args[0]]
            pc += 2
            if (arg1 != 0):
                pc = memory[args[1]]
        elif (opcode == 6):
            args = getArgs(instr, 2, memory, pc, rc)
            arg1 = memory[args[0]]
            pc += 2
            if (arg1 == 0):
                pc = memory[args[1]]
        elif (opcode == 7):
            args = getArgs(instr, 3, memory, pc, rc)
            arg1 = memory[args[0]]
            arg2 = memory[args[1]]
            addr = args[2]
            pc += 3
            if (arg1 < arg2):
                result = 1
            else:
                result = 0
        elif (opcode == 8):
            args = getArgs(instr, 3, memory, pc, rc)
            arg1 = memory[args[0]]
            arg2 = memory[args[1]]
            addr = args[2]
            pc += 3
            if (arg1 == arg2):
                result = 1
            else:
                result = 0
        elif (opcode == 9):
            args = getArgs(instr, 1, memory, pc, rc)
            arg1 = memory[args[0]]
            pc += 1
            rc += arg1
        else:
            print ("Unknown opcode")
            exit()

        if addr != -1:
            memory[addr] = result

    return [status.HALT, 0, outputs]

memory = getInitialMemory()
out = run(memory, [1])
print("Part 1: %s" % out[2][0])

memory = getInitialMemory()
out = run(memory, [2])
print("Part 2: %s" % out[2][0])

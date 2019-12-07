from enum import Enum

class status(Enum):
    HALT = 1
    GET_INPUT = 2

def getInitialMemory():
    f = open("7dec-input", 'r')
    file_content = f.read()
    f.close()
    memory = list(map(int, file_content.split(',')))
    return memory

def getOpcode(inst):
    inst_str = str(inst)
    if len(inst_str) < 2:
        inst_str = "0" + inst_str
    opcode = int(inst_str[-2:])
    return opcode

def getArgs(inst, num, memory, pc):
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
            arg = memory[memory[pc]]
        elif (mode == 1):
            arg = memory[pc]
        else:
            print("Unknown mode: %d at %d" % (mode, pc))
            exit()

        args.append(arg)
        pc += 1
    return args

def run(memory, inputs, pc = 0):
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
            args = getArgs(instr, 2, memory, pc)
            arg1 = args[0]
            arg2 = args[1]
            pc += 2
            addr = memory[pc]
            pc += 1
            result = arg1 + arg2
        elif (opcode == 2):
            args = getArgs(instr, 2, memory, pc)
            arg1 = args[0]
            arg2 = args[1]
            pc += 2
            addr = memory[pc]
            pc += 1
            result = arg1 * arg2
        elif (opcode == 3):
            if len(inputs) == 0:
                return [status.GET_INPUT, pc - 1, outputs]
            result = int(inputs.pop(0))
            addr = memory[pc]
            pc += 1
        elif (opcode == 4):
            args = getArgs(instr, 1, memory, pc)
            arg1 = args[0]
            pc += 1
            outputs.append(arg1)
        elif (opcode == 5):
            args = getArgs(instr, 2, memory, pc)
            arg1 = args[0]
            pc += 2
            if (arg1 != 0):
                pc = args[1]
        elif (opcode == 6):
            args = getArgs(instr, 2, memory, pc)
            arg1 = args[0]
            pc += 2
            if (arg1 == 0):
                pc = args[1]
        elif (opcode == 7):
            args = getArgs(instr, 2, memory, pc)
            arg1 = args[0]
            arg2 = args[1]
            pc += 2
            addr = memory[pc]
            pc += 1
            if (arg1 < arg2):
                result = 1
            else:
                result = 0
        elif (opcode == 8):
            args = getArgs(instr, 2, memory, pc)
            arg1 = args[0]
            arg2 = args[1]
            pc += 2
            addr = memory[pc]
            pc += 1
            if (arg1 == arg2):
                result = 1
            else:
                result = 0
        else:
            print ("Unknown opcode")
            exit()

        if addr != -1:
            memory[addr] = result

    return [status.HALT, 0, outputs]

phases = range(5)
largest = 0
sequence = []

for amp_a in phases:
    for amp_b in phases:
        for amp_c in phases:
            for amp_d in phases:
                for amp_e in phases:
                    if len(list(set([amp_a, amp_b, amp_c, amp_d, amp_e]))) == 5:
                        out = run(getInitialMemory(), [amp_a, 0])
                        out = run(getInitialMemory(), [amp_b] + out[2])
                        out = run(getInitialMemory(), [amp_c] + out[2])
                        out = run(getInitialMemory(), [amp_d] + out[2])
                        out = run(getInitialMemory(), [amp_e] + out[2])

                        if out[2][0] > largest:
                            largest = out[2][0]
                            sequence = [amp_a, amp_b, amp_c, amp_d, amp_e]

print("Part 1: %d" % largest)

phases = range(5, 10)
largest = 0
sequence = []

for amp_a in phases:
    for amp_b in phases:
        for amp_c in phases:
            for amp_d in phases:
                for amp_e in phases:
                    phase = [amp_a, amp_b, amp_c, amp_d, amp_e]
                    if len(list(set(phase))) == 5:
                        mems = [getInitialMemory() for m in range(5)]
                        outs = [[status.GET_INPUT, 0, [phase[m]]] for m in range(5)]
                        new_output = 0
                        final_round = False
                        i = 0

                        while not final_round:
                            for i in range(5):
                                out = outs[i]
                                out[2] += [new_output]
                                out = run(mems[i], out[2], out[1])
                                new_output = out[2][0]
                                out[2] = []
                                outs[i] = out

                                if out[0] == status.HALT:
                                    final_round = True

                        if new_output > largest:
                            largest = new_output
                            sequence = [amp_a, amp_b, amp_c, amp_d, amp_e]

print("Part 2: %d" % largest)

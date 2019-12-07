
def getInitialMemory():
    f = open("5dec-input", 'r')
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

def run(memory):
    pc = 0

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
            result = int(input("Provide input: "))
            addr = memory[pc]
            pc += 1
        elif (opcode == 4):
            args = getArgs(instr, 1, memory, pc)
            arg1 = args[0]
            pc += 1
            print(arg1)
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

memory = getInitialMemory()

run(memory)

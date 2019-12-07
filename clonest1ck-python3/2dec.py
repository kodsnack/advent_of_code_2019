
def getInitialMemory():
    f = open("2dec-input", 'r')
    file_content = f.read()
    f.close()
    memory = list(map(int, file_content.split(',')))
    return memory


def run(memory):
    pc = 0

    while(memory[pc] != 99):
        opcode = memory[pc]
        arg1 = memory[memory[pc + 1]]
        arg2 = memory[memory[pc + 2]]
        result = 0

        if (opcode == 1):
            result = arg1 + arg2
        elif (opcode == 2):
            result = arg1 * arg2
        else:
            print ("Unknown opcode")
            exit()

        memory[memory[pc + 3]] = result
        pc += 4

# Part 1
memory = getInitialMemory()

memory[1] = 12
memory[2] = 2
run(memory)

print("Part 1 result: %d" % memory[0])

# Part 2
for i in range(100):
    for j in range(100):
        memory = getInitialMemory()
        memory[1] = i
        memory[2] = j

        run(memory)

        if memory[0] == 19690720:
            print("Part 2 result: %d" % ((100 * i) + j))
            exit()


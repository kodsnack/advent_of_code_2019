## Start of header boilerplate #################################################

from aocbase import readInput
import re

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x, fp=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:f(x, ff, fp), inp.splitlines()))

## End of header boilerplate ###################################################

def isAdd(mem, address):
    return mem[address] == 1

def add(mem, address):
    target = mem[address+3]
    arg1 = mem[address+1]
    arg2 = mem[address+2]
    mem[target] = mem[arg1] + mem[arg2]

def isMul(mem, address):
    return mem[address] == 2

def mul(mem, address):
    target = mem[address+3]
    arg1 = mem[address+1]
    arg2 = mem[address+2]
    mem[target] = mem[arg1] * mem[arg2]

def isHalt(mem, address):
    return mem[address] == 99

def run(pinp, noun, verb):
    mem = {address: value for address, value in enumerate(pinp[0][0])}
    mem[1] = noun
    mem[2] = verb
    address = 0
    while not isHalt(mem, address):
        if isAdd(mem, address):
            add(mem, address)
            address += 4
        elif isMul(mem, address):
            mul(mem, address)
            address += 4
        else:
            raise RuntimeError("Invalid instruction {} at address {}".format(mem[address], address))
    return mem[0]

def part1(pinp):
    return run(pinp, 12, 2)

def part2(pinp):
    for noun in range(100):
        for verb in range(100):
            if run(pinp, noun, verb)==19690720:
                return 100 * noun + verb
    return "Not found"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################    
    parseInp = fileParse(inp, ff=lambda x:tuple(map(int, x.split(','))))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

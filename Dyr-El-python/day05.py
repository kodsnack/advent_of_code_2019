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

def readVal(address, argNo, mem):
    if mem[address]//(10*10**argNo)%10==1:
        return mem[address+argNo]
    else:
        return mem[mem[address+argNo]]

def addI(mem, address):
    target = mem[address+3]
    arg1 = readVal(address, 1, mem)
    arg2 = readVal(address, 2, mem)
    mem[target] = arg1 + arg2
    return address+4

def mulI(mem, address):
    target = mem[address+3]
    arg1 = readVal(address, 1, mem)
    arg2 = readVal(address, 2, mem)
    mem[target] = arg1 * arg2
    return address+4

def inpI(mem, address):
    target = mem[address+1]
    print(">", end='')
    val = int(input())
    mem[target] = val
    return address+2

def outI(mem, address):
    arg1 = readVal(address, 1, mem)
    print(':',arg1)
    return address + 2

def jitI(mem, address):
    arg1 = readVal(address, 1, mem)
    arg2 = readVal(address, 2, mem)
    if arg1 != 0:
        return arg2
    return address+3

def jifI(mem, address):
    arg1 = readVal(address, 1, mem)
    arg2 = readVal(address, 2, mem)
    if arg1 == 0:
        return arg2
    return address+3

def ltI(mem, address):
    target = mem[address+3]
    arg1 = readVal(address, 1, mem)
    arg2 = readVal(address, 2, mem)
    if arg1 < arg2:
        mem[target] = 1
    else:
        mem[target] = 0
    return address+4

def eqI(mem, address):
    target = mem[address+3]
    arg1 = readVal(address, 1, mem)
    arg2 = readVal(address, 2, mem)
    if arg1 == arg2:
        mem[target] = 1
    else:
        mem[target] = 0
    return address+4

def isHalt(mem, address):
    return mem[address] == 99

def getInstr(mem, address):
    return mem[address] % 100

ilist = {1:addI, 2:mulI, 3:inpI, 4:outI, 5:jitI, 6:jifI, 7:ltI, 8:eqI}

def run(pinp, noun, verb):
    mem = {address: value for address, value in enumerate(pinp[0][0])}
    address = 0
    while not isHalt(mem, address):
        # print(address, mem[address],mem[address+1],mem[address+2],mem[address+3])
        instr = getInstr(mem, address)
        if instr in ilist:
            address = ilist[instr](mem, address)
        else:
            raise RuntimeError("Invalid instruction {} at address {}".format(mem[address], address))

def part1(pinp):
    return run(pinp, 12, 2)

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=lambda x:tuple(map(int, x.split(','))))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    part1(parseInp)
    #print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

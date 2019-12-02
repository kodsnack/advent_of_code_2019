## Start of header boilerplate #################################################

from aocbase import readInput
import re

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp):
    return tuple(map(int, inp.split(',')))

## End of header boilerplate ###################################################

def run(pinp, n, v):
    mem = dict()
    a = 0
    for i in pinp:
        mem[a] = i
        a += 1
    a = 0
    mem[1] = n
    mem[2] = v
    while mem.get(a, 0)!=99:
        if mem.get(a,0) == 1:
            #print("Add", a, mem.get(a,0), mem.get(a+1,0), mem.get(a+2,0), mem.get(a+3,0))
            mem[mem.get(a+3,0)] = (mem.get(mem.get(a+1,0), 0) 
                                 + mem.get(mem.get(a+2,0), 0))
            a += 4
        elif mem.get(a, 0) == 2:
            #print("Mul", a, mem.get(a,0), mem.get(a+1,0), mem.get(a+2,0), mem.get(a+3,0))
            mem[mem.get(a+3,0)] = (mem.get(mem.get(a+1,0), 0) 
                                 * mem.get(mem.get(a+2,0), 0))
            a += 4
        else:
            print("Error", a, mem[a])
            raise "Error"

    return mem[0]

def part1(pinp):
    return run(pinp, 12, 2)

def part2(pinp):
    for n in range(100):
        for v in range(100):
            print(n, v)
            if run(pinp, n, v)==19690720:
                return 100*n+v
    return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    #inp = """1,9,10,3,2,3,11,0,99,30,40,50"""

    ## Update for input specifics ##############################################    
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10]) + 
          ('...' if len(parseInp)>10 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

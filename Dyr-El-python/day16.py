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

def seq2(d, offset):
    i = -offset
    s = -1
    d += 1
    while i < 0:
        i += d * 2
        s = s * (-1)
    while True:
        for j in range(i, i+d):
            yield j, s
        i = i + d*2
        s *= -1

def sgd(a, b):
    if b==0:
        return a
    return sgd(b, a%b)

cache = {}
def calcDigit(d, i, l, mul):
    if (d, i, mul) in cache:
        return cache[d, i, mul]
    if i == 0:
        return l[d%mul]
    cycle = 4*(d+1)
    offsets = sgd(cycle, len(l))
    noLeft = mul
    noPerCycle = mul // cycle
    smm = 0
    offset = 1
    while True:
        print(i, d, cycle, noLeft, noPerCycle)
        sm = 0
        for j, s in seq2(d, offset):
            print(j, s)
            input()
            if j>=len(l):
                break
            sm += s * calcDigit(j, i-1, l, mul)
        if noLeft % cycle == 0:
            smm += noPerCycle * sm
            noLeft -= noPerCycle
        else:
            smm += (noPerCycle + 1) * sm
            noLeft -= (noPerCycle + 1)
        if noLeft == 0:
            break
        offset += cycle // offsets
    cache[d, i, mul] = abs(smm)%10
    return abs(smm)%10

def part1(pinp):
    l = [int(c) for c in pinp[0][0]]
    s = 0
    for i in range(8):
        s = s * 10 + calcDigit(i, 100, l, 1)
    return s

def part2(pinp):
    global cache
    cache = dict()
    l = [int(c) for c in pinp[0][0]]*10000
    offset = int(''.join(map(str, pinp[0][0][:7])))
    print(offset, len(l))
    s = 0
    for i in range(offset, offset+8):
        s = s * 10 + calcDigit(i, 100, l, 10000)
    return s

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

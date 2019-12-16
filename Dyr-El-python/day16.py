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

def seq2(d):
    i = -1
    s = 1
    while True:
        for j in range(i+d, i+d*2):
            yield j, s
        i = i + d*2
        s *= -1

def seq(d):
    i = 1
    v = (0, 1, 0, -1)
    while True:
        yield v[(i//(d+1)) % 4]
        i = i + 1

def falt(l, d):
    return sum(map(lambda x, y:x*y, l, seq(d)))

def part1(pinp):
    l = [int(c) for c in pinp[0][0]]
    s = 0
    for i in range(8):
        s = s * 10 + calcDigit(i, 100, l)
    return s

cache = {}
def calcDigit(d, i, l):
    if (d, i) in cache:
        return cache[d, i]
    sum = 0
    if i == 0:
        return l[d]
    for j, s in seq2(d):
        print(i, d, j, s)
        if j>=len(l):
            break
        sum += s * calcDigit(j, i-1, l)
    cache[d, i] = abs(sum)%10
    if len(cache)%10000 == 0:
        print(len(cache))
    return abs(sum)%10

def part2(pinp):
    global cache
    # cache = dict()
    # l = [int(c) for c in pinp[0][0]]*10000
    # offset = int(''.join(map(str, pinp[0][0][:7])))
    # print(offset, len(l))
    # s = 0
    # for i in range(offset, offset+8):
    #     s = s * 10 + calcDigit(i, 100, l)
    # return s

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

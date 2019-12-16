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

def part1(pinp):
    l = [int(c) for c in pinp[0][0]]
    inputLen = len(l)
    for _ in range(100):
        l = [abs(sum((0, 1, 0, -1)[(i+1)//(k+1)%4]*l[i] for i in range(inputLen))) % 10 for k in range(inputLen)]
    return ''.join(map(str, l[:8]))

def part2(pinp):
    l = (10000*[int(c) for c in pinp[0][0]])[int(pinp[0][0][:7]):]
    for _ in range(100):
        for i in range(len(l)-1, 0, -1):
            l[i-1] = (l[i-1]+l[i]) % 10
    return ''.join(str(x) for x in l[:8])

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

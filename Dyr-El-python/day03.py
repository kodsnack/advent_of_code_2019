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
    x, y = 0, 0
    st = 0
    sp = {(0, 0):0}
    for s in pinp[0][0]:
        d = {'U':(0, 1),'D':(0, -1), 'L': (-1, 0), 'R':(1, 0)}[s[0]]
        for i in range(int(s[1:])):
            x, y = x+d[0], y+d[1]
            st += 1
            if (x, y) not in sp:
                sp[(x, y)]=st
        print(len(sp))
    xp = set()
    x, y = 0, 0
    st = 0
    for s in pinp[1][0]:
        d = {'U':(0, 1),'D':(0, -1), 'L': (-1, 0), 'R':(1, 0)}[s[0]]
        for i in range(int(s[1:])):
            x, y = x+d[0], y+d[1]
            st += 1
            if (x, y) in sp:
                print(x, y)
                xp.add(st+sp[(x, y)])
    return min(xp)

def part2(pinp):
    return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """R75,D30,R83,U83,L12,D49,R71,U7,L72
# U62,R66,U55,R34,D71,R55,D58,R83"""
#     inp = """R8,U5,L5,D3
# U7,R6,D4,L4"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=lambda x:tuple(x.split(',')))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

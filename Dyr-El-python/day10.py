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

def sgd(a, b):
    if b==0:
        return a
    return sgd(b, a%b)

def part1(pinp):
    s = set()
    for y, r in enumerate(pinp):
        for x, c in enumerate(r[0]):
            if c=='#':
                s.add((x, y))
    maxseen = 0
    for a1 in s:
        seen = 0
        for a2 in s:
            coll = False
            if a1 == a2:
                continue
            dx, dy = a2[0]-a1[0], a2[1]-a1[1]
            sg = sgd(abs(dx), abs(dy))
            dxs, dys = dx//sg, dy//sg
            x, y = a1
            x, y = x + dxs, y + dys
            coll = False
            while (x, y) != a2:
                if (x, y) in s:
                    coll = True
                    break
                x, y = x + dxs, y + dys
            if not coll:
                seen += 1
        if seen > maxseen:
            print(a1, seen)
            maxseen = max(seen, maxseen)
    return maxseen

import math

def part2(pinp):
    s = set()
    mx, my = 0, 0
    for y, r in enumerate(pinp):
        my = max(y, my)
        for x, c in enumerate(r[0]):
            mx = max(x, my)
            if c=='#':
                s.add((x, y))
    sx, sy = 20, 20
    s.remove((sx, sy))
    s2 = set()
    for a in s:
        x, y = a
        dx, dy = x-sx, y-sy
        sg = sgd(abs(dx), abs(dy))
        nx, ny = dx//sg, dy//sg
        s2.add((nx, ny))
    for a in s2:
        x, y = a[0], -a[1]
        print(x, y)
        if x!=0:
            print(-math.atan2(y,x)/math.pi*180)
    return "<solution2>"

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

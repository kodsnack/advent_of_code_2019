## Start of header boilerplate #################################################

from aocbase import readInput
import re
import collections

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x, fp=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:f(x, ff, fp), inp.splitlines()))

## End of header boilerplate ###################################################

def loadMap(s):
    x, y = 0, 0
    d = dict()
    keys = dict()
    locks = dict()
    for line in s:
        for c in line[0]:
            if c == '.':
                d[x,y] = c
            elif 'a' <= c <= 'z':
                keys[x,y] = c
                d[x,y] = '.'
            elif 'A' <= c <= 'Z':
                locks[c.lower()] = (x, y)
            elif c == '@':
                d[x,y] = '.'
                start = (x, y)
            x += 1
        y += 1
        x = 0
    return d, keys, locks, start

def colorMap(mp, ke, start):
    pos = collections.deque()
    col = dict()
    col[start] = 0
    pos.append(start)
    res = list()
    while len(pos) > 0:
        p = pos.popleft()
        x, y = p
        for dx, dy in [(1, 0),(-1, 0),(0, 1),(0, -1)]:
            nx, ny = x+dx, y+dy
            if (nx, ny) in col:
                continue 
            if mp.get((nx, ny), '') == '.':
                if (nx, ny) in ke:
                    res.append((col[x,y]+1, ke[nx, ny], (nx, ny)))
                else:
                    col[nx,ny] = col[x,y] + 1
                    pos.append((nx, ny))
    return sorted(res)

def solvePussle(mp, ke, lo, st, stepsSoFar=0, mnSteps=10**10):
    if len(ke) == 0:
        return stepsSoFar
    for keyLn, keyC, keyPos in colorMap(mp, ke, st):
        if stepsSoFar+keyLn >= mnSteps: continue
        if keyC in lo:
            lockPos = lo[keyC]
        else:
            lockPos = None
        if lockPos != None: mp[lockPos] = '.'
        del ke[keyPos]
        if lockPos != None: del lo[keyC]
        # print(len(mp), len(ke), len(lo), keyPos, keyC)
        stps = solvePussle(mp, ke, lo, keyPos, stepsSoFar+keyLn, mnSteps)
        print(stps)
        # input()
        mnSteps = min(mnSteps, stps)
        if lockPos != None: lo[keyC] = lockPos
        ke[keyPos] = keyC
        if lockPos != None: mp[lockPos] = ''
    return mnSteps

def part1(pinp):
    mp, ke, lo, st = loadMap(pinp)
    print(len(mp), len(ke), len(lo), st)
    print(colorMap(mp, ke, st))
    return solvePussle(mp, ke, lo, st)

def part2(pinp):
    return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    inp = """########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

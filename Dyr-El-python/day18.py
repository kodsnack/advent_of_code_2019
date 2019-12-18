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

def loadMap4(s):
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
    x, y = start
    del d[x-1, y]
    del d[x+1, y]
    del d[x, y-1]
    del d[x, y+1]
    del d[x,y]
    return d, keys, locks, ((x-1, y-1),(x+1, y-1),(x-1, y+1),(x+1, y+1),)

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

cache = dict()
mnSteps = 10 ** 10
def solvePussle(mp, ke, lo, st, stepsSoFar=0):
    global mnSteps
    ks = frozenset(sorted(ke.values()))
    if (ks, st) in cache and cache[ks, st] < stepsSoFar:
        return mnSteps
    else:
        cache[ks, st] = stepsSoFar
    if len(ke) == 0:
        return stepsSoFar
    if stepsSoFar > mnSteps:
        return mnSteps
    if len(ks) > (20):
        print(len(ks), ''.join(ks), stepsSoFar, mnSteps)
    for keyLn, keyC, keyPos in colorMap(mp, ke, st):
        if keyC in lo:
            lockPos = lo[keyC]
        else:
            lockPos = None
        if lockPos != None: mp[lockPos] = '.'
        del ke[keyPos]
        if lockPos != None: del lo[keyC]
        mnSteps = min(solvePussle(mp, ke, lo, keyPos, stepsSoFar+keyLn), mnSteps)
        if lockPos != None: lo[keyC] = lockPos
        ke[keyPos] = keyC
        if lockPos != None: mp[lockPos] = ''
    return mnSteps

cache4 = dict()
mnSteps4 = 10 ** 10
def solvePussle4(mp, ke, lo, st, stepsSoFar=0):
    global mnSteps4
    ks = frozenset(sorted(ke.values()))
    if (ks, st) in cache4 and cache4[ks, st] < stepsSoFar:
        return mnSteps4
    else:
        cache4[ks, st] = stepsSoFar
    if len(ke) == 0:
        return stepsSoFar
    if stepsSoFar > mnSteps4:
        return mnSteps4
    if len(ks) > 16:
        print(len(ks), ''.join(ks), stepsSoFar, mnSteps4)
    it = list()
    for i in range(4):
        for keyLn, keyC, keyPos in colorMap(mp, ke, st[i]):
            it.append((keyLn, i, keyC, keyPos))
    it.sort()
    for keyLn, i, keyC, keyPos in it:
        if keyC in lo:
            lockPos = lo[keyC]
        else:
            lockPos = None
        if lockPos != None: mp[lockPos] = '.'
        del ke[keyPos]
        if lockPos != None: del lo[keyC]
        newSp = st[:i] + (keyPos,) + st[i+1:]
        mnSteps4 = min(solvePussle4(mp, ke, lo, newSp, stepsSoFar+keyLn), mnSteps4)
        if lockPos != None: lo[keyC] = lockPos
        ke[keyPos] = keyC
        if lockPos != None: mp[lockPos] = ''
    return mnSteps4

def part1(pinp):
    mp, ke, lo, st = loadMap(pinp)
    # return solvePussle(mp, ke, lo, st)

def part2(pinp):
    mp, ke, lo, st = loadMap4(pinp)
    print(len(mp), len(ke), len(lo), st)
    print(colorMap(mp, ke, st[0]))
    print(colorMap(mp, ke, st[1]))
    print(colorMap(mp, ke, st[2]))
    print(colorMap(mp, ke, st[3]))
    return solvePussle4(mp, ke, lo, st)

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """#############
# #DcBa.#.GhKl#
# #.###...#I###
# #e#d#.@.#j#k#
# ###C#...###J#
# #fEbA.#.FgHi#
# #############"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

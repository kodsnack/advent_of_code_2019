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
    mp = dict()
    ps = dict()
    for line in s:
        for c in line[0]:
            mp[x, y] = c
            if 'a' <= c <= 'z':
                ps[c] = (x, y)
            elif c == '@':
                ps['0'] = (x, y)
            x += 1
        y += 1
        x = 0
    return mp, ps

def loadMap4(s):
    mp, ps = loadMap(s)
    x, y = ps['0']
    mp[x+1, y] = '#'
    mp[x-1, y] = '#'
    mp[x, y+1] = '#'
    mp[x, y-1] = '#'
    mp[x, y] = '#'
    ps['0'] = (x-1, y-1)
    ps['1'] = (x+1, y-1)
    ps['2'] = (x-1, y+1)
    ps['3'] = (x+1, y+1)
    return mp, ps

def colorMap(mp, start):
    positions = collections.deque()
    col = {start: (0, set())}
    links = {}
    positions.append((start, 0, set()))
    while len(positions) > 0:
        curPos, curSteps, curLocks = positions.popleft()
        x, y = curPos
        for dx, dy in [(1, 0),(-1, 0),(0, 1),(0, -1)]:
            nxtPos = (x+dx, y+dy)
            if nxtPos in col: continue
            c = mp.get(nxtPos, '#') 
            if c == '#': continue
            nxtSteps = curSteps + 1
            if c.islower():
                links[c] = (nxtSteps, curLocks)
            if c.isupper():
                nxtLocks = curLocks | {c.lower()}
            else:
                nxtLocks = curLocks
            col[nxtPos] = (nxtSteps, nxtLocks)
            positions.append((nxtPos, nxtSteps, nxtLocks))
    return links

cache = dict()
def solvePussle(mp, source, remaining):
    if len(remaining) == 0:
        return 0
    cacheKey = source + ''.join(sorted(remaining))
    if cacheKey in cache:
        return cache[cacheKey]
    minDist = 10**10
    for target in remaining:
        trgSteps, trgLock = mp[source][target]
        if trgSteps >= minDist: continue
        if not trgLock.isdisjoint(remaining): continue
        restSteps = solvePussle(mp, target, remaining - set(target))
        minDist = min(minDist, restSteps+trgSteps)
    cache[cacheKey] = minDist
    return minDist

cache4 = dict()
def solvePussle4(mp, source, remaining):
    if len(remaining) == 0:
        return 0
    cacheKey = source + ''.join(sorted(remaining))
    if cacheKey in cache4:
        return cache4[cacheKey]
    minDist = 10**10
    for target in remaining:
        for sourceC in source:
            if target in mp[sourceC]:
                trgSteps, trgLock = mp[sourceC][target]
                break
        if trgSteps >= minDist: continue
        if not trgLock.isdisjoint(remaining): continue
        nxtSource = source.replace(sourceC, target)
        restSteps = solvePussle4(mp, nxtSource, remaining - set(target))
        minDist = min(minDist, restSteps+trgSteps)
    cache4[cacheKey] = minDist
    return minDist

def part1(pinp):
    mp, goals = loadMap(pinp)
    d = dict()
    for g in goals.keys():
        d[g] = colorMap(mp, goals[g])
    return solvePussle(d, '0', set(d['0'].keys()))

def part2(pinp):
    mp, goals = loadMap4(pinp)
    d = dict()
    for g in goals.keys():
        d[g] = colorMap(mp, goals[g])
    return solvePussle4(d, '0123', (set(d['0'].keys()) 
                                  | set(d['1'].keys())
                                  | set(d['2'].keys())
                                  | set(d['3'].keys())))

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

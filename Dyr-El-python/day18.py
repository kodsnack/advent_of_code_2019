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
    links = {mp[start]: (0, set())}
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
                links[c] = (nxtPos, nxtSteps, curLocks)
            if c.isupper():
                nxtLocks = curLocks | {c.lower()}
            else:
                nxtLocks = curLocks
            col[nxtPos] = (nxtSteps, nxtLocks)
            positions.append((nxtPos, nxtSteps, nxtLocks))
    return links

# cache = dict()
minv = 10**10
def solvePussle4(d, visited):
    global minv
    
    maxLen = len(set([a[1] for a in d.keys()]))
    keySort = sorted([(d[x,y][0][1], x, y) for x,y in d.keys()])
    print(keySort)
    q = collections.deque()
    q.append((set(), startState, startSteps))
    while len(q) > 0:
        lockSet, pos, steps = q.pop()
        for _, start, goal in keySort:
            if start not in pos: continue
            if goal in lockSet: continue
            if not d[start, goal][0][0] <= lockSet: continue
            newLock = lockSet | set(goal)
            newPos = pos.replace(start, goal)
            newSteps = steps+d[start, goal][0][1]
            if newSteps > minv: continue
            key = ''.join(newLock) + newPos
            # if key in cache and cache[key] <= newSteps: continue
            # cache[key] = newSteps
            q.append((newLock, newPos, newSteps))
            if len(newLock) == maxLen and newSteps < minv:
                minv = newSteps
                print(">>>",minv)
            # states.append((key[:-4], key[-4:], newSteps))
    return minv

def part1(pinp):
    mp, goals = loadMap(pinp)
    d = dict()
    for g in goals.keys():
        d[g] = colorMap(mp, goals[g])
    print(d)
    # return solvePussle(mp, ke, lo, st)

def part2(pinp):
    mp, ps = loadMap4(pinp)
    d = dict()
    for start in "0123abcdefghijklmnopqrstuvwxyz":
        if start not in ps: continue
        for goal in "abcdefghijklmnopqrstuvwxyz":
            if goal == start or goal not in ps: continue
            cm = colorMap(mp, ps[start], ps[goal])
            if len(cm)>0:
                d[start, goal] = cm
    return solvePussle4(d, "0123", 0)

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    inp = """#############
#DcBa.#.GhKl#
#.###...#I###
#e#d#.@.#j#k#
###C#...###J#
#fEbA.#.FgHi#
#############"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

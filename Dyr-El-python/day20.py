## Start of header boilerplate #################################################

from aocbase import readInput
import re
import collections

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp):
    return list(inp.splitlines())
## End of header boilerplate ###################################################

def mapMaze(s):
    mz = dict()
    for y,line in enumerate(s):
        for x, c in enumerate(line):
            if c != ' ':
                mz[x,y] = c
    return mz

def findTeleporters(mz):
    loc = dict()
    for (x, y), value in mz.items():
        if isinstance(value, str) and value.isupper():
            for dx, dy in ((0,1),(1,0),(-1,0),(0,-1)):
                nxtChar = mz.get((x+dx, y+dy), '')
                if isinstance(nxtChar, tuple): continue 
                prevChar = mz.get((x-dx, y-dy), '')
                if (nxtChar.isupper() and prevChar == '.'):
                    name = (mz[min(x, x+dx),min(y, y+dy)] + 
                            mz[max(x+dx, x), max(y+dy, y)])
                    if name not in loc:
                        loc[name] = list()
                    loc[name].append(((x, y), (x-dx, y-dy)))
    for l in loc.values():
        if len(l) != 2: continue
        mz[l[0][0]] = l[1][1]
        mz[l[1][0]] = l[0][1]
    return loc['AA'][0][0], loc['ZZ'][0][0]

def findTeleportersD(mz):
    start, stop = findTeleporters(mz)
    minx = min((c[0] for c in mz.keys()))
    maxx = max((c[0] for c in mz.keys()))
    miny = min((c[1] for c in mz.keys()))
    maxy = max((c[1] for c in mz.keys()))
    portalLocations = [key for key in mz.keys() if isinstance(mz[key], tuple)]
    for x, y in portalLocations:
        mz[x, y] = mz[x, y] + (abs(x - minx) < 2 or abs(x - maxx) < 2 or abs(y - miny) < 2 or abs(y - maxy) < 2,)
    return start, stop

def colorMap(mz, start, stop):
    d = collections.deque()
    d.append(start)
    v = dict()
    v[start] = 0
    while len(d)>0:
        cur = d.popleft()
        x, y = cur
        for dx, dy in ((0,1),(1,0),(-1,0),(0,-1)):
            nx, ny = x+dx, y+dy
            if (nx, ny) == stop:
                return v[x,y] -1
            if (nx, ny) not in mz:
                continue
            if isinstance(mz[nx, ny], tuple):
                nx, ny =  mz[nx, ny]
            if mz[nx, ny] != '.':
                continue
            if (nx, ny) in v and v[x, y] + 1 >= v[nx, ny]:
                continue
            v[nx, ny] = v[x, y]+1
            d.append((nx, ny))

def colorMapD(mz, start, stop):
    maxLevel = len([t for t in mz.values() if isinstance(t, tuple)])//2
    d = collections.deque()
    d.append(start+(0, ))
    v = dict()
    v[start+(0, )] = 0
    while len(d)>0:
        cur = d.popleft()
        x, y, lvl = cur
        for dx, dy in ((0,1),(1,0),(-1,0),(0,-1)):
            nx, ny, nlvl = x+dx, y+dy, lvl
            if (nx, ny) == stop and lvl == 0:
                return v[x,y,lvl] - 1
            if (nx, ny) not in mz:
                continue
            if isinstance(mz[nx, ny], tuple):
                nx, ny, outer =  mz[nx, ny]
                if outer:
                    if lvl == 0:
                        continue
                    nlvl -= 1
                else:
                    if lvl == maxLevel:
                        continue
                    nlvl += 1
            if mz[nx, ny] != '.':
                continue
            if (nx, ny, nlvl) in v and v[x, y, lvl] + 1 >= v[nx, ny, nlvl]:
                continue
            v[nx, ny, nlvl] = v[x, y, lvl]+1
            d.append((nx, ny, nlvl))

def part1(pinp):
    mz = mapMaze(pinp)
    start, stop = findTeleporters(mz)
    return colorMap(mz, start, stop)

def part2(pinp):
    mz = mapMaze(pinp)
    start, stop = findTeleportersD(mz)
    return colorMapD(mz, start, stop)

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

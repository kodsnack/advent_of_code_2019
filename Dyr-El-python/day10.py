## Start of header boilerplate #################################################

from aocbase import readInput
import re
import math

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

def parseAsteroids(pinp):
    s = set()
    for y, r in enumerate(pinp):
        for x, c in enumerate(r[0]):
            if c=='#':
                s.add((x, y))
    return s

def findBestAsteroid(asteroids):
    maxseen = 0
    for a1 in asteroids:
        seen = 0
        for a2 in asteroids:
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
                if (x, y) in asteroids:
                    coll = True
                    break
                x, y = x + dxs, y + dys
            if not coll:
                seen += 1
        if seen > maxseen:
            maxseen = max(seen, maxseen)
            maxx = a1[0]
            maxy = a1[1]
    return maxseen, maxx, maxy

def part1(pinp):
    return findBestAsteroid(parseAsteroids(pinp))[0]

def angs(a):
    x, y = a
    ang = math.atan2(x,-y)/math.pi*180
    if ang < 0:
        ang += 360
    return ang

def part2(pinp, numberKilled):
    s = parseAsteroids(pinp)
    _, sx, sy = findBestAsteroid(s)
    s.remove((sx, sy))
    s2 = set()
    for a in s:
        x, y = a
        dx, dy = x-sx, y-sy
        sg = sgd(abs(dx), abs(dy))
        nx, ny = dx//sg, dy//sg
        s2.add((nx, ny))
    l2 = list(s2)
    l2.sort(key=angs)
    print(sx, sy)
    #print(l2)
    killed = 0
    while(True):
        for a in l2:
            x, y = sx, sy
            print(x, y, a)
            while 0 <= x <= 40 and 0 <= y <= 40:
                x, y = x+a[0], y+a[1]
                if (x, y) in s:
                    s.remove((x, y))
                    killed += 1
                    print(killed, x, y)
                    if killed == numberKilled:
                        return x, y
                    break
    return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """.#....#####...#..
# ##...##.#####..##
# ##...#...#.#####.
# ..#.....#...###..
# ..#.#.....#....##"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp, 200)))

## End of footer boilerplate ###################################################

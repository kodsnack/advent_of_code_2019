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
    for placement in asteroids:
        seen = 0
        for destination in asteroids:
            if placement == destination:
                continue
            hidden = False
            placeX, placeY = placement
            destX, destY = destination
            deltaX, deltaY = destX-placeX, destY-placeY
            sg = sgd(abs(deltaX), abs(deltaY))
            xVector, yVector = deltaX//sg, deltaY//sg
            x, y = placeX + xVector, placeY + yVector
            hidden = False
            while not hidden and (x, y) != destination:
                hidden = hidden or (x, y) in asteroids
                x, y = x + xVector, y + yVector
            if not hidden:
                seen += 1
        if seen > maxseen:
            maxseen, bestPlacement = seen, placement
    return maxseen, bestPlacement

def part1(pinp):
    return findBestAsteroid(parseAsteroids(pinp))[0]

def angs(a):
    x, y = a
    ang = math.atan2(x,-y)
    if ang < 0:
        ang += 2*math.pi
    return ang

def part2(pinp, numberKilled):
    asteroids = parseAsteroids(pinp)
    _, baseAsteroid = findBestAsteroid(asteroids)
    baseX, baseY = baseAsteroid
    asteroids.remove(baseAsteroid)
    vectors = set()
    for asteroid in asteroids:
        x, y = asteroid
        deltaX, deltaY = x-baseX, y-baseY
        sg = sgd(abs(deltaX), abs(deltaY))
        vectorX, vectorY = deltaX//sg, deltaY//sg
        vectors.add((vectorX, vectorY))
    vectors = list(vectors)
    vectors.sort(key=angs)
    killed = 0
    while(True):
        for v in vectors:
            x, y = baseX, baseY
            vx, vy = v
            while 0 <= x <= 40 and 0 <= y <= 40:
                x, y = x + vx, y + vy
                if (x, y) in asteroids:
                    asteroids.remove((x, y))
                    killed += 1
                    if killed == numberKilled:
                        return x*100 + y
                    break

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp, 200)))

## End of footer boilerplate ###################################################

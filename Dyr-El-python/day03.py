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

def steps(s, x, y):
    d = {'U':(0, 1), 'D':(0, -1), 'L':(-1, 0), 'R':(1, 0)}[s[0]]
    for i in range(int(s[1:])):
        x, y = x + d[0], y + d[1]
        yield x, y

def traverse(stepsToTake):
    x, y = 0, 0
    noSteps = 0
    for line in stepsToTake:
        for x, y in steps(line, x, y):
            noSteps += 1
            yield x, y, noSteps

def traceFirst(stepsToTake):
    d = dict()
    for x, y, stepCount in traverse(stepsToTake):
        if (x, y) not in d:
            d[x, y] = stepCount
    return d

def distanceToStart(dist, x, y, noSteps):
    return abs(x)+abs(y)

def noOfStepsToStart(dist, x, y, noSteps):
    return dist[x, y] + noSteps

def findCollisions(trace, stepsToTake, distFunc):
    dists = set()
    for x, y, stepCount in traverse(stepsToTake):
        if (x, y) in trace:
            dists.add(distFunc(trace, x, y, stepCount))
    return min(dists)

def part1(pinp):
    trace1 = traceFirst(pinp[0][0])
    return findCollisions(trace1, pinp[1][0], distanceToStart)

def part2(pinp):
    trace1 = traceFirst(pinp[0][0])
    return findCollisions(trace1, pinp[1][0], noOfStepsToStart)

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=lambda x:tuple(x.split(',')))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

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

def initialPositions(pinp):
    return [list(x) for x in pinp]

def initialVelocities(pinp):
    return [list(x) for x in pinp]

def stepVelocities(v, p):
    for i1 in range(len(v)):
        for i2 in range(len(v)):
            if i1 == i2: continue
            for dim in range(3):
                if p[i1][dim] > p[i2][dim]:
                    v[i1][dim] -= 1
                elif p[i1][dim] < p[i2][dim]:
                    v[i1][dim] += 1

def stepPositions(v, p):
    for i in range(len(p)):
        for dim in range(3):
            p[i][dim] += v[i][dim]

def calcKineticEnergy(v, i):
    return sum(map(abs, v[i]))

def calcPotentialEnergy(p, i):
    return sum(map(abs, p[i]))

def calcTotalEnergy(v, p):
    return sum((calcKineticEnergy(v, i)*calcPotentialEnergy(p, i) for i in range(len(v))))

def part1(pinp, steps):
    p = initialPositions(pinp)
    v = [[0, 0, 0] for x in pinp]
    for i in range(steps):
        stepVelocities(v, p)
        stepPositions(v, p)
    return calcTotalEnergy(v, p)

def sgd(a, b):
    if b==0:
        return a
    return sgd(b, a%b)

class CycleDetector:
    def __init__(self, pos, vel):
        self.pos = [planetPos[:] for planetPos in pos]
        self.vel = [planetVel[:] for planetVel in vel]
        self.cycles = [0, 0, 0]
        self.steps = 0
    def equal(self, pos, vel, dim):
        return ([p[dim] for p in self.pos] == [p[dim] for p in pos] and
                [v[dim] for v in self.vel] == [v[dim] for v in vel])
    def step(self, p, v):
        self.steps += 1
        noCycles = 0
        for dim in range(3):
            if self.cycles[dim] > 0 or self.equal(p, v, dim):
                noCycles += 1
                if self.cycles[dim] == 0:
                    self.cycles[dim] = self.steps
        return noCycles
    def totalCycles(self):
        cycles = self.cycles[0]
        for cycle in self.cycles[1:]:
            cycles = cycles * cycle // sgd(cycle, cycles)
        return cycles

def part2(pinp):
    p = initialPositions(pinp)
    v = [[0, 0, 0] for x in pinp]
    cd = CycleDetector(p, v)
    while True:
        stepVelocities(v, p)
        stepPositions(v, p)
        if cd.step(p, v) == 3:
            return cd.totalCycles()

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, fp=re.compile(r"^<x=([-0-9]*), y=([-0-9]*), z=([-0-9]*)>$"), ff=int)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp, 1000)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

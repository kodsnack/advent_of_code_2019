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

def code(d):
    return sum((2**i for i in range(25) if (i%5,i//5) in d))

def parse(s):
    d = set()
    for y, line in enumerate(s):
        for x, c in enumerate(line[0]):
            if c == '#':
                d.add((x, y))
    return d

def parse3d(s):
    d = set()
    for y, line in enumerate(s):
        for x, c in enumerate(line[0]):
            if c == '#':
                d.add((x, y, 0))
    return d

def step(d):
    dd = set()
    for x in range(5):
        for y in range(5):
            nc = 0
            for dx, dy in ((1,0),(0,1),(-1,0),(0,-1)):
                if (x+dx, y+dy) in d:
                    nc += 1
            if (x,y) in d:
                if nc == 1:
                    dd.add((x, y))
            else:
                if nc in (1, 2):
                    dd.add((x, y))
    return dd

def neighbours(x, y, z):
    for dx, dy in ((0,1),(1,0),(-1,0),(0,-1)):
        nx, ny = x+dx, y+dy
        if (nx, ny) == (2, 2):
            for i in range(5):
                if (x, y) == (1, 2):
                    yield (0, i, z+1)
                elif (x,y) == (3, 2):
                    yield (4, i, z+1)
                elif (x,y) == (2, 1):
                    yield (i, 0, z+1)
                elif (x,y) == (2, 3):
                    yield (i, 4, z+1)
        elif (0<=nx<=4) and (0<=ny<=4):
            yield (nx, ny, z)
        elif nx < 0:
            yield (1, 2, z-1)
        elif nx > 4:
            yield (3, 2, z-1)
        elif ny < 0:
            yield (2, 1, z-1)
        elif ny > 4:
            yield (2, 3, z-1)

def step3(d):
    dd = set()
    s = set()
    for xo, yo, zo in d:
        for x, y, z in neighbours(xo, yo, zo):
            s.add((x, y, z))
        s.add((xo, yo, zo))
    for x, y, z in s:
        nc = 0
        for dx, dy, dz in neighbours(x , y ,z):
            if (dx, dy, dz) in d:
                nc += 1
        if (x, y, z) in d:
            if nc == 1:
                dd.add((x,y,z))
        else:
            if nc in (1, 2):
                dd.add((x,y,z))
    return dd

def part1(pinp):
    s = set()
    d = parse(pinp)
    while True:
        c = code(d)
        if c in s:
            return c
        s.add(c)
        d = step(d)
    return "<solution1>"

def part2(pinp):
    d = parse3d(pinp)
    for i in range(200):
        d = step3(d)
    return len(d)

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

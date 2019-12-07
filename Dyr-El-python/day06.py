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

def countOrbits(obj, objmap):
    if obj not in objmap:
        return 0, 0
    elif objmap[obj] not in objmap:
        return 1, 0
    else:
        direct, indirect = countOrbits(objmap[obj], objmap)
        return 1, indirect + 1

def drawMap(objects):
    objmap = dict()
    allObjects = set()
    for baseObj, orbObj in objects:
        objmap[orbObj] = baseObj
        allObjects.add(baseObj)
        allObjects.add(orbObj)
    return objmap, allObjects

def part1(pinp):
    objmap, allObjects = drawMap(pinp)
    direct, indirect = 0, 0
    for obj in allObjects:
        objDir, objIndir = countOrbits(obj, objmap)
        direct += objDir
        indirect += objIndir
    return direct + indirect

def part2(pinp):
    objmap, allObjects = drawMap(pinp)
    yourDistances = dict()
    obj = 'YOU'
    dist = 0
    while obj in objmap:
        obj = objmap[obj]
        yourDistances[obj] = dist
        dist += 1
    santa = 'SAN'
    dist = 0
    while santa in objmap:
        santa = objmap[santa]
        if santa in yourDistances:
            return dist + yourDistances[santa]
        dist += 1
    return 0

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, fp=re.compile(r"(.*)\)(.*)"))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

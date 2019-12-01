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

def fuelForMass(mass):
    return mass//3 - 2

def fuelForAllMass(mass):
    fuelForThisMass = fuelForMass(mass)
    if fuelForThisMass <= 0:
        return 0
    return fuelForThisMass + fuelForAllMass(fuelForThisMass)

def part1(pinp):
    return sum([fuelForMass(mass[0]) for mass in pinp])

def part2(pinp):
    return sum([fuelForAllMass(mass[0]) for mass in pinp])

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()

    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=int)

    print("Input is '" + str(parseInp[:10]) + 
          ('...' if len(parseInp)>10 else '') + "'")
    
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

## Start of header boilerplate #################################################

from aocbase import readInput
import re

def parseComp(a):
    aa = a.strip().split()
    return int(aa[0]), aa[1]

def parseLine(line):
    f = line.find(" => ")
    aa = tuple(map(parseComp, line[:f].split(",")))
    ab = parseComp(line[f+4:])
    return aa, ab

def fileParse(inp, fp=re.compile(r"^(.*)$")):
    return tuple(map(parseLine, inp.splitlines()))

## End of header boilerplate ###################################################

def oreNeeded(m, comp, amount):
    ore = 0
    orderList = {comp:amount}
    stock = dict()
    while len(orderList) > 0:
        firstc = list(orderList.keys())[0]
        firsta = orderList[firstc]
        del orderList[firstc]
        if firstc == "ORE":
            ore += firsta
            continue
        if firstc not in stock:
            stock[firstc] = 0
        while firsta > stock[firstc]:
            ingr = m[firstc][0][0]
            iam = m[firstc][0][1]
            stockNeeded = firsta - stock[firstc]
            noNeeded = (stockNeeded + iam - 1) // iam
            for a, i in ingr:
                orderList[i] = orderList.get(i, 0) + a * noNeeded
            stock[firstc] += iam * noNeeded
        stock[firstc] -= firsta
    return ore

def part1(pinp):
    m = dict()
    for ingr, result in pinp:
        resa, resc = result
        if resc not in m:
            m[resc] = list()
        else:
            print(resc)
        m[resc].append((ingr, resa))
    return oreNeeded(m, "FUEL", 1)

def part2(pinp):
    m = dict()
    for ingr, result in pinp:
        resa, resc = result
        if resc not in m:
            m[resc] = list()
        else:
            print(resc)
        m[resc].append((ingr, resa))
    fuelmin = 1
    fuelmax = 2
    maxOre = 1000000000000
    fuel = 0
    while fuelmin != fuelmax:
        while oreNeeded(m, "FUEL", fuelmax)<maxOre:
            fuelmax *= 2
        fuel = (fuelmin + fuelmax) // 2
        if fuel == fuelmin:
            break
        ore = oreNeeded(m, "FUEL", fuel)
        if ore < maxOre:
            fuelmin = fuel
        if ore > maxOre:
            fuelmax = fuel
        if ore == maxOre:
            return fuel
    return fuelmin

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
# 17 NVRVD, 3 JNWZP => 8 VPVL
# 53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
# 22 VJHF, 37 MNCFX => 5 FWMGM
# 139 ORE => 4 NVRVD
# 144 ORE => 7 JNWZP
# 5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
# 5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
# 145 ORE => 6 MNCFX
# 1 NVRVD => 8 CXFTF
# 1 VJHF, 6 MNCFX => 4 RFSQX
# 176 ORE => 6 VJHF"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

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

def part1(pinp):
    return "<solution1>"

def part2(pinp):
    return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()

    
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10]) + 
          ('...' if len(parseInp)>10 else '') + "'")
    print("Solution to part 1: " + part1(parseInp))
    print("Solution to part 2: " + part2(parseInp))

## End of footer boilerplate ###################################################

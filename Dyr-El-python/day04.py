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

doublePattern = re.compile(r".*([0-9])\1.*")
def test1(s):
    if s == ''.join(sorted(s)) and doublePattern.match(s):
        return True
    return False

def part1(r1, r2):
    tot = 0
    for i in range(int(r1), int(r2)+1):
        tot += 1 if test1(str(i)) else 0
    return tot

def test2(s):
    if s != ''.join(sorted(s)):
        return False
    for c in set(s):
        if s.count(c*4)>0:
            continue
        if s.count(c*3)==1 and s.count(c*2)==1:
            continue
        if s.count(c*2)==0:
            continue
        return True
    return False

def part2(r1, r2):
    tot = 0
    for i in range(int(r1), int(r2)+1):
        tot += 1 if test2(str(i)) else 0
    return tot

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    
    ## Update for input specifics ##############################################

    print("Solution to part 1: {}".format(part1("165432", "707912")))
    print("Solution to part 2: {}".format(part2("165432", "707912")))

## End of footer boilerplate ###################################################

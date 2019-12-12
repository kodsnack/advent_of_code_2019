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
    p = [list(x) for x in pinp]
    v = [[0, 0, 0] for x in pinp]
    for i in range(1000):
        for p1 in range(len(v)):
            for p2 in range(len(v)):
                for d in range(3):
                    if p[p1][d] > p[p2][d]:
                        v[p1][d] = (v[p1][d]-1)
                    elif p[p1][d] < p[p2][d]:
                        v[p1][d] = (v[p1][d]+1)
        tot = 0
        for p1 in range(len(p)):
            totk, totp = 0, 0
            for d in range(3):
                p[p1][d] += v[p1][d]
                totk += abs(v[p1][d])
                totp += abs(p[p1][d])
            tot += totk*totp
    print(tot)
    return "<solution1>"

def sgd(a, b):
    if b==0:
        return a
    return sgd(b, a%b)

def part2(pinp):
    p = [list(x) for x in pinp]
    v = [[0, 0, 0] for x in pinp]
    s = [dict(), dict(), dict()]
    c = [None, None, None]
    count = 0
    for i in range(10000000):
        for p1 in range(len(v)):
            for p2 in range(len(v)):
                for d in range(3):
                    if p[p1][d] > p[p2][d]:
                        v[p1][d] = (v[p1][d]-1)
                    elif p[p1][d] < p[p2][d]:
                        v[p1][d] = (v[p1][d]+1)
        tot = 0
        for p1 in range(len(p)):
            totk, totp = 0, 0
            for d in range(3):
                p[p1][d] += v[p1][d]
                totk += abs(v[p1][d])
                totp += abs(p[p1][d])
            tot += totk*totp
        for d in range(3):
            l = list()
            for p1 in range(len(p)):
                l.append(p[p1][d])
                l.append(v[p1][d])
            tp = tuple(l)
            if tp in s[d] and c[d]==None:
                c[d] = (i, i - s[d][tp])
                count += 1
            else:
                s[d][tp] = i
        if count==3:
            cc = c[0][0]*c[1][0]
            cc = cc//sgd(c[0][0],c[1][0])
            ccc = cc*c[2][0]
            ccc = ccc//sgd(cc,c[2][0])

            print(ccc)
            break
    return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """<x=-1, y=0, z=2>
# <x=2, y=-10, z=-7>
# <x=4, y=-8, z=8>
# <x=3, y=5, z=-1>"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, fp=re.compile(r"^<x=([-0-9]*), y=([-0-9]*), z=([-0-9]*)>$"), ff=int)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

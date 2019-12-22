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

def unity():
    return ((1, 0), (0, 1))

def mult(a, b, ds):
    return (((a[0][0]*b[0][0]+a[0][1]*b[1][0])%ds,
             (a[0][0]*b[0][1]+a[0][1]*b[1][1])%ds),
            ((a[1][0]*b[0][0]+a[1][1]*b[1][0])%ds,
             (a[1][0]*b[0][1]+a[1][0]*b[1][1])%ds))

def dealWithInc(inc, ds, t):
    return ((t[0][0]*inc%ds,t[0][1]*inc%ds), (0, 1))

def dealIntoNewStack(ds, t):
    return (((-t[0][0])%ds, -t[0][1]-1), (0, 1))

def cut(place, ds, t):
    return ((t[0][0], (t[0][1]-place)%ds), (0, 1))

def createTranform(pinp, ds):
    t = unity()
    ds = 10007
    for i, s in enumerate(pinp):
        if s[0].startswith("deal with increment"):
            t = dealWithInc(int(s[0].split(' ')[3]), ds, t)
        elif s[0].startswith("cut"):
            t = cut(int(s[0].split(' ')[1]), ds, t)
        elif s[0].startswith("deal into new stack"):
            t = dealIntoNewStack(ds, t)
        else:
            print("Huh")
    return t

def transformCard(c, ds, t):
    return (t[0][0]*c+t[0][1]*1)%ds

def part1(pinp):
    card = 2019
    deckSize = 10007
    t = createTranform(pinp, deckSize)
    return transformCard(card, deckSize, t)

def powerTransform(t, n, ds):
    ret = unity()
    pm = t
    while n > 0:
        if n % 2 == 1:
            ret = mult(pm, ret, ds)
        pm = mult(pm, pm, ds)
        n = n // 2
    return ret

def part2(pinp):
    card = 2020
    deckSize = 119315717514047
    times = 101741582076661
    t = createTranform(pinp, deckSize)
    t = powerTransform(t, times, deckSize)
    print(t)    
    return (2020 * factor + offset)%deckSize

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

# End of footer boilerplate ###################################################

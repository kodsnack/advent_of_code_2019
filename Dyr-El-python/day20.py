## Start of header boilerplate #################################################

from aocbase import readInput
import re
import collections

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp):
    return list(inp.splitlines())
## End of header boilerplate ###################################################

def mapMaze(s):
    mz = dict()
    for y,line in enumerate(s):
        for x, c in enumerate(line):
            if c != ' ':
                mz[x,y] = c
    return mz

def findTeleporters(mz):
    loc = dict()
    for x,y in mz.keys():
        if isinstance(mz[x, y], str) and mz[x,y].isupper():
            print(mz[x,y])
            for dx, dy in ((0,1),(1,0),(-1,0),(0,-1)):
                cc = mz.get((x+dx, y+dy), '')
                if isinstance(cc, tuple): continue 
                if (cc.isupper() and 
                    mz.get((x-dx, y-dy), '') == '.'):
                    nm = (mz[min(x, x+dx),min(y, y+dy)] + 
                          mz[max(x+dx, x), max(y+dy, y)])
                    if nm in loc:
                        loc[nm].append((x, y, x-dx, y-dy))
                    else:
                        loc[nm] = [(x, y, x-dx, y-dy)]
    for l in loc.values():
        if len(l) != 2: continue
        mz[l[0][0],l[0][1]] = (l[1][2], l[1][3])
        mz[l[1][0],l[1][1]] = (l[0][2], l[0][3])
    print(loc)
    return loc['AA'][0][:2], loc['ZZ'][0][:2]

def colorMap(mz, start, stop):
    d = collections.deque()
    d.append(start)
    v = dict()
    v[start] = 0
    while len(d)>0:
        cur = d.popleft()
        x, y = cur
        for dx, dy in ((0,1),(1,0),(-1,0),(0,-1)):
            nx, ny = x+dx, y+dy
            if (nx, ny) == stop:
                return v[x,y] -1
            if (nx, ny) not in mz:
                continue
            if isinstance(mz[nx, ny], tuple):
                nx, ny =  mz[nx, ny]
            if mz[nx, ny] != '.':
                continue
            if (nx, ny) in v and v[x, y] + 1 >= v[nx, ny]:
                continue
            v[nx, ny] = v[x, y]+1
            d.append((nx, ny))
    for y in range(100):
        for x in range(100):
            if (x, y) in v:
                c = '+'
            elif (x, y) in mz:
                c = mz[x,y]
                if isinstance(c, tuple):
                    c = '*'
                if len(c) != 1:
                    c = '?'
            else:
                c = ' '
            print(c, end='')
        print()

def part1(pinp):
    mz = mapMaze(pinp)
    start, stop = findTeleporters(mz)
    print(start, stop)
    return colorMap(mz, start, stop)

def part2(pinp):
    return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """                   A               
#                    A               
#   #################.#############  
#   #.#...#...................#.#.#  
#   #.#.#.###.###.###.#########.#.#  
#   #.#.#.......#...#.....#.#.#...#  
#   #.#########.###.#####.#.#.###.#  
#   #.............#.#.....#.......#  
#   ###.###########.###.#####.#.#.#  
#   #.....#        A   C    #.#.#.#  
#   #######        S   P    #####.#  
#   #.#...#                 #......VT
#   #.#.#.#                 #.#####  
#   #...#.#               YN....#.#  
#   #.###.#                 #####.#  
# DI....#.#                 #.....#  
#   #####.#                 #.###.#  
# ZZ......#               QG....#..AS
#   ###.###                 #######  
# JO..#.#.#                 #.....#  
#   #.#.#.#                 ###.#.#  
#   #...#..DI             BU....#..LF
#   #####.#                 #.#####  
# YN......#               VT..#....QG
#   #.###.#                 #.###.#  
#   #.#...#                 #.....#  
#   ###.###    J L     J    #.#.###  
#   #.....#    O F     P    #.#...#  
#   #.###.#####.#.#####.#####.###.#  
#   #...#.#.#...#.....#.....#.#...#  
#   #.#####.###.###.#.#.#########.#  
#   #...#.#.....#...#.#.#.#.....#.#  
#   #.###.#####.###.###.#.#.#######  
#   #.#.........#...#.............#  
#   #########.###.###.#############  
#            B   J   C               
#            U   P   P               """
    
    ## Update for input specifics ##############################################
    print(inp)
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

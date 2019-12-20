## Start of header boilerplate #################################################

from aocbase import readInput
import re
import collections

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x, fp=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:f(x, ff, fp), inp.splitlines()))

## End of header boilerplate ###################################################

def mapMaze(s):
    mz = dict()
    for y,line in enumerate(s):
        for x, c in enumerate(line[0]):
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
    print(loc)
    return loc['AA'], loc['ZZ']

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
                return v[x,y] + 1
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
    inp = """         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       """
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

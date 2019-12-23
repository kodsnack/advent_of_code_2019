import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def draw(cells, lowest, highest):
    for y in range(lowest, highest + 1):
        row = []
        for x in range(lowest, highest + 1):
            c = ' '
            if (y, x) in cells and cells[(y, x)] != 0:
                tile = cells[(y, x)]
                if tile == 1:
                    c = 'X'
                elif tile == 2:
                    c = 'S'
                elif tile == 3:
                    c = '_'
                elif tile == 4:
                    c = 'o'
            row.append(c)
        print(''.join(row))


def solve(d):
    computer = Computer(d, 0, 2)
    count = 0
    retcode = 0
    loops = 0
    lowest = 100
    highest = -100
    out = [-1, -1, -1]
    score = 0
    ballx = 0
    padx = 0

    cells = {}
    
    while retcode != -1:
        retcode, retval = computer.step()
        if retcode != 1:
            continue
   
        if out[:2] == [-1, 0] and loops % 3 == 2:
            score = retval
        else:
            out[loops % 3] = retval

        if loops % 3 == 2:                
            cells[(out[1], out[0])] = out[2]
            if retval == 3:
                padx = out[0]
            if retval == 4:
                ballx = out[0]
        else:
            lowest = min(lowest, retval)
            highest = max(highest, retval)

        computer.set_input(0 if ballx == padx else -1 if ballx < padx else 1)
        
        loops += 1

    return score
    

def read_and_solve():
    with open('input_13.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
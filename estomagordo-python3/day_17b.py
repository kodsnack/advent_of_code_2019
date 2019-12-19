import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def get_grid(computer):
    grid = []
    row = []

    while True:
        retcode, retval = computer.get_output()

        if retcode == -1:
            break

        if retval == 10:
            grid.append(row)
            row = []

        else:
            row.append(retval)

    if row:
        grid.append(row)
    
    if not grid[-1]:
        grid = grid[:-1]
    
    height = len(grid)
    width = len(grid[0])

    return grid, height, width


def get_start(grid, height, width):
    by, bx = -1, -1    
    direction = -1

    for y in range(height):
        for x in range(width):
            if grid[y][x] in (35, 46):
                continue

            by = y
            bx = x

            botchar = chr(grid[y][x])
            
            if botchar == '^':
                direction = 0
            elif botchar == '>':
                direction = 1
            elif botchar == '<':
                direction = 2
            else:
                direction = 3

            return by, bx, direction


def get_scaffolds(grid, height, width):
    scaffolds = set()

    for y in range(height):
        for x in range(width):
            if grid[y][x] != 46:
                scaffolds.add((y, x))

    return scaffolds


def get_moves(grid, height, width):
    directions = ((-1, 0), (0, 1), (1, 0), (0, -1))
    
    by, bx, direction = get_start(grid, height, width)
    currdir = directions[direction]
    scaffolds = get_scaffolds(grid, height, width)

    seen = { by, bx }
    moves = []

    while len(seen) != len(scaffolds):
        ny, nx = by + currdir[0], bx + currdir[1]
        n2y, n2x = by + 2 * currdir[0], bx + 2 * currdir[1]

        if 0 <= ny < height and 0 <= nx < width and (ny, nx) in scaffolds and ((ny, nx) not in seen or (n2y, n2x) not in seen):
            seen.add((ny, nx))
            moves.append('F')
            by, bx = ny, nx
            continue

        ry, rx = by + directions[(direction + 1) % 4][0], bx + directions[(direction + 1) % 4][1]
        ly, lx = by + directions[(direction - 1) % 4][0], bx + directions[(direction - 1) % 4][1]

        right_good = 0 <= ry < height and 0 <= rx < width and (ry, rx) in scaffolds and (ry, rx) not in seen
        left_good = 0 <= ly < height and 0 <= lx < width and (ly, lx) in scaffolds and (ly, lx) not in seen

        if right_good:
            moves.append('R')
            direction = (direction + 1) % 4
            currdir = directions[direction]
            continue
        if left_good:
            moves.append('L')
            direction = (direction - 1) % 4
            currdir = directions[direction]
            continue
        for y in range(height):
            for x in range(width):
                if (y, x) in seen:
                    grid[y][x] = 88
        return moves

    return moves


def infuse_numbers(moves):
    new = []
    currlen = 0

    for c in moves:
        if c in 'RL':
            if currlen > 0:
                lens = str(currlen)
                for cc in list(lens):
                    new.append(cc)
                currlen = 0
            new.append(c)
        else:
            currlen += 1

    if currlen > 0:
        lens = str(currlen)
        for cc in list(lens):
            new.append(cc)

    return new


def solve(d):
    exploreputer = Computer(d, 1)

    grid, height, width = get_grid(exploreputer)
    moves = get_moves(grid, height, width)

    funca = 'R,8,L,12,R,8'
    funcb = 'L,10,L,10,R,8'
    funcc = 'L,12,L,12,L,10,R,10'
    movement = 'A,A,B,C,B,C,B,A,C,A'

    inputs = []

    for c in movement:
        inputs.append(ord(c))

    inputs.append(10)

    for c in funca:
        inputs.append(ord(c))

    inputs.append(10)

    for c in funcb:
        inputs.append(ord(c))

    inputs.append(10)

    for c in funcc:
        inputs.append(ord(c))

    inputs.append(10)

    inputs.append(ord('n'))
    inputs.append(10)

    moveputer = Computer(d, inputs[0], 2)
    inppos = 1
    retcode, retval = 0, 0
    dust = 0

    while retcode != -1:
        retcode, retval = moveputer.step()

        if retcode == 0 and retval == 3:
            if inppos < len(inputs):
                moveputer.set_input(inputs[inppos])
            inppos += 1

        if retcode == 1:
            dust = retval

    return dust
    

def read_and_solve():
    with open('input_17.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
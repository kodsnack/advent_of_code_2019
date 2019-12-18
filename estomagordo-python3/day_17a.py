import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve(d):
    grid = []
    row = []

    computer = Computer(d, 1)

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

    score = 0
    
    if not grid[-1]:
        grid = grid[:-1]
    
    height = len(grid)
    width = len(grid[0])

    for y in range(height):
        for x in range(width):
            if grid[y][x] == 46:
                continue

            if y == 0 or x == 0 or y == height - 1 or x == width - 1:
                continue

            if grid[y - 1][x] == 46:
                continue
            if grid[y + 1][x] == 46:
                continue
            if grid[y][x - 1] == 46:
                continue
            if grid[y][x + 1] == 46:
                continue

            score += y * x
            
    return score
    

def read_and_solve():
    with open('input_17.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
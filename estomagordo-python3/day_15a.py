import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from copy import deepcopy
from intcode import Computer


def bfs(walls, oy, ox):
    seen = { (0, 0) }
    frontier = [(0, 0, 0)]

    while True:
        steps, y, x = heappop(frontier)
        
        if y == oy and x == ox:
            return steps
        
        if (y + 1, x) not in walls | seen:
            seen.add((y + 1, x))
            heappush(frontier, (steps + 1, y + 1, x))
        if (y - 1, x) not in walls | seen:
            seen.add((y - 1, x))
            heappush(frontier, (steps + 1, y - 1, x))
        if (y, x + 1) not in walls | seen:
            seen.add((y, x + 1))
            heappush(frontier, (steps + 1, y, x + 1))
        if (y, x - 1) not in walls | seen:
            seen.add((y, x - 1))
            heappush(frontier, (steps + 1, y, x - 1))


def next_pos(y, x, command):
    if command == 1:
        return y - 1, x
    if command == 2:
        return y + 1, x
    if command == 3:
        return y, x - 1
    return y, x + 1


def explore(d):   
    ymin = -100
    ymax = 100
    xmin = -100
    xmax = 100
    
    walls = set()
    seen = { 0, 0 }
    frontier = [(Computer(d, 1), 1, 0, 0)]
    oy, ox = -1, -1

    for computer, command, y, x in frontier:
        _, retval = computer.get_output()
        ny, nx = next_pos(y, x, command)

        if retval == 0:
            walls.add((ny, nx))
            continue
        elif retval == 2:
            oy = ny
            ox = nx

        if y > ymin and (y - 1, x) not in walls | seen:
            newcomp = deepcopy(computer)
            computer.set_input(1)
            frontier.append((newcomp, 1, ny, nx))
        if y < ymax and (y + 1, x) not in walls | seen:
            newcomp = deepcopy(computer)
            computer.set_input(2)
            frontier.append((newcomp, 2, ny, nx))
        if x > xmin and (y, x - 1) not in walls | seen:
            newcomp = deepcopy(computer)
            computer.set_input(3)
            frontier.append((newcomp, 3, ny, nx))
        if x < xmax and (y, x + 1) not in walls | seen:
            newcomp = deepcopy(computer)
            computer.set_input(4)
            frontier.append((newcomp, 4, ny, nx))

        
    return walls, oy, ox


def solve(d):
    walls, oy, ox = explore(d)

    return bfs(walls, oy, ox)
    

def read_and_solve():
    with open('input_15.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
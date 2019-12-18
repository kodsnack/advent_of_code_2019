import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def get_moves(d, height, width, keys, y, x):
    moves = []

    oy, ox = y, x
    y -= 1
    while d[y][x] in '@.' and (y == oy or x == 0 or d[y][x - 1] == '#') and (y == oy or x == width - 1 or d[y][x + 1] == '#') and y > 0 and d[y - 1][x] != '#':
        y -= 1
    if y > 0 and d[y][x] != '#':
        steps = abs(y - oy) + abs(x - ox)
        c = d[y][x]
        if c in '.@':
            newkeys = set(keys)
            moves.append([steps, y, x, newkeys])
        elif c.islower():
            newkeys = set(keys)
            newkeys.add(c)
            moves.append([steps, y, x, newkeys])
        elif c.isupper() and c.lower() in keys:
            newkeys = set(keys)            
            moves.append([steps, y, x, newkeys])
    y, x = oy, ox
    y += 1
    while d[y][x] in '@.' and (y == oy or x == 0 or d[y][x - 1] == '#') and (y == oy or x == width - 1 or d[y][x + 1] == '#') and y > 0 and d[y + 1][x] != '#':
        y += 1
    if y < height - 1 and d[y][x] != '#':
        steps = abs(y - oy) + abs(x - ox)
        c = d[y][x]
        if c in '.@':
            newkeys = set(keys)
            moves.append([steps, y, x, newkeys])
        elif c.islower():
            newkeys = set(keys)
            newkeys.add(c)
            moves.append([steps, y, x, newkeys])
        elif c.isupper() and c.lower() in keys:
            newkeys = set(keys)            
            moves.append([steps, y, x, newkeys])
    y, x = oy, ox
    x -= 1
    while d[y][x] in '@.' and (x == ox or y == 0 or d[y - 1][x] == '#') and (x == ox or y == height - 1 or d[y + 1][x] == '#') and y > 0 and d[y][x - 1] != '#':
        x -= 1
    if x > 0 and d[y][x] != '#':
        steps = abs(y - oy) + abs(x - ox)
        c = d[y][x]
        if c in '.@':
            newkeys = set(keys)
            moves.append([steps, y, x, newkeys])
        elif c.islower():
            newkeys = set(keys)
            newkeys.add(c)
            moves.append([steps, y, x, newkeys])
        elif c.isupper() and c.lower() in keys:
            newkeys = set(keys)            
            moves.append([steps, y, x, newkeys])
    y, x = oy, ox
    x += 1
    while d[y][x] in '@.' and (x == ox or y == 0 or d[y - 1][x] == '#') and (x == ox or y == height - 1 or d[y + 1][x] == '#') and y > 0 and d[y][x + 1] != '#':
        x += 1
    if x > 0 and d[y][x] != '#':
        steps = abs(y - oy) + abs(x - ox)
        c = d[y][x]
        if c in '.@':
            newkeys = set(keys)
            moves.append([steps, y, x, newkeys])
        elif c.islower():
            newkeys = set(keys)
            newkeys.add(c)
            moves.append([steps, y, x, newkeys])
        elif c.isupper() and c.lower() in keys:
            newkeys = set(keys)            
            moves.append([steps, y, x, newkeys])

    return moves


def heuristic(grid, keys, allkeys, y, x):
    dist = 0

    for key in allkeys.keys() - keys:
        dist = max(dist, helpers.manhattan((y, x), allkeys[key]))

    return dist


def get_places_of_interest(grid, height, width, keys, y, x):
    places = {}
    seen = { (y, x) }
    frontier = [[0, y, x]]

    for steps, y, x in frontier:
        for my, mx in helpers.get_moves(height, width, y, x):
            if (my, mx) not in seen and grid[my][mx] != '#':
                if grid[my][mx].isupper() and grid[my][mx].lower() not in keys:
                    places[(my, mx)] = steps
                elif grid[my][mx].islower() and grid[my][mx] not in keys:
                    places[(my, mx)] = steps
                else:
                    frontier.append([steps + 1, my, mx])

    return places


def solve(d):
    d = list(map(list, d))

    height = len(d)
    width = len(d[0])

    sy, sx = -1, -1

    for y in range(height):
        for x in range(width):
            if d[y][x] == '@':
                sy, sx = y, x

    d[sy - 1][sx - 1] = '@'
    d[sy - 1][sx + 1] = '@'
    d[sy + 1][sx + 1] = '@'
    d[sy + 1][sx - 1] = '@'

    d[sy - 1][sx] = '#'
    d[sy + 1][sx] = '#'
    d[sy][sx - 1] = '#'
    d[sy][sx + 1] = '#'
    d[sy][sx] = '#'
    
    allkeys = {}

    for y in range(height):
        for x in range(width):
            c = d[y][x]

            if c.isalpha() and c.islower():
                allkeys[c] = (y, x)

    # h = heuristic(d, set(), allkeys, py, px)
    # frontier = [[h, 0, py, px, set()]]
    frontier = [[0, sy - 1, sx - 1, sy - 1, sx + 1, sy + 1, sx + 1, sy + 1, sx - 1, set()]]
    seen = { (sy - 1, sx - 1, sy - 1, sx + 1, sy + 1, sx + 1, sy + 1, sx - 1, str(set())) }
    largest = 0

    while True:
        # score, steps, y, x, keys = heappop(frontier)
        steps, y1, x1, y2, x2, y3, x3, y4, x4, keys = heappop(frontier)

        if len(keys) == len(allkeys):
            return steps

        if steps > largest:
            largest = steps
            print(largest, len(keys), len(allkeys))
        
        moves = get_moves(d, height, width, keys, y1, x1)

        for dsteps, dy, dx, dkeys in moves:
            tup = (dy, dx, y2, x2, y3, x3, y4, x4, str(dkeys))
            if tup not in seen:
                seen.add(tup)
                heappush(frontier, (steps + dsteps, dy, dx, y2, x2, y3, x3, y4, x4, dkeys))

        moves = get_moves(d, height, width, keys, y2, x2)

        for dsteps, dy, dx, dkeys in moves:
            tup = (y1, x1, dy, dx, y3, x3, y4, x4, str(dkeys))
            if tup not in seen:
                seen.add(tup)
                heappush(frontier, (steps + dsteps, y1, x1, dy, dx, y3, x3, y4, x4, dkeys))

        moves = get_moves(d, height, width, keys, y3, x3)

        for dsteps, dy, dx, dkeys in moves:
            tup = (y1, x1, y2, x2, dy, dx, y4, x4, str(dkeys))
            if tup not in seen:
                seen.add(tup)
                heappush(frontier, (steps + dsteps, y1, x1, y2, x2, dy, dx, y4, x4, dkeys))

        moves = get_moves(d, height, width, keys, y4, x4)

        for dsteps, dy, dx, dkeys in moves:
            tup = (y1, x1, y2, x2, y3, x3, dy, dx, str(dkeys))
            if tup not in seen:
                seen.add(tup)
                heappush(frontier, (steps + dsteps, y1, x1, y2, x2, y3, x3, dy, dx, dkeys))        
    

def read_and_solve():
    with open('input_18.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
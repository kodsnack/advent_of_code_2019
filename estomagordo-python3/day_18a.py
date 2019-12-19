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


def heuristic(grid, clique, distances, keys, y, x):
    distance = 0

    ci = 0
    while clique[ci] != (y, x):
        ci += 1

    for cj, pair in enumerate(clique):
        if cj == ci:
            continue

        cy, cx = pair
        cell = grid[cy][cx]
        if cell.islower() and cell not in keys:
            distance = max(distance, distances[ci][cj])

    return distance


def get_clique(grid, height, width, oy, ox):
    clique = [(oy, ox)]
    seen = { (oy, ox) }
    frontier = [[oy, ox]]

    for y, x in frontier:
        for my, mx in helpers.get_moves(height, width, y, x):
            if (my, mx) not in seen and grid[my][mx] != '#':
                if grid[my][mx].isalpha():
                    clique.append((my, mx))
                seen.add((my, mx))
                frontier.append([my, mx])

    return clique


def measure_distances(grid, height, width, clique, oy, ox):    
    distances = [0 for _ in range(len(clique))]
    seen = { (oy, ox) }

    frontier = [[0, oy, ox]]

    for distance, y, x in frontier:
        for my, mx in helpers.get_moves(height, width, y, x):            
            cell = grid[my][mx]
            if cell == '#':
                continue
            if (my, mx) in seen:
                continue
            seen.add((my, mx))
            frontier.append([distance + 1, my, mx])
            if (my, mx) in clique:
                cp = 0
                for ci, coords in enumerate(clique):
                    cp = ci
                    if (my, mx) == coords:
                        break
                distances[cp] = distance + 1

    return distances


def get_between(grid, height, width, y1, x1, y2, x2):
    seen = { (y1, x1) }
    frontier = [[set(), y1, x1]]

    for noted, y, x in frontier:
        if y == y2 and x == x2:
            return noted

        for ny, nx in helpers.get_moves(height, width, y, x):
            if grid[ny][nx] == '#' or (ny, nx) in seen:
                continue

            seen.add((ny, nx))
            newnoted = set(noted)

            if grid[ny][nx].isalpha() and (ny != y2 or nx != x2):
                newnoted.add(grid[ny][nx])

            frontier.append([newnoted, ny, nx])


def solve(d):
    d = list(map(list, d))

    height = len(d)
    width = len(d[0])

    sy, sx = -1, -1
    
    allkeys = {}

    for y in range(height):
        for x in range(width):
            c = d[y][x]

            if c.isalpha() and c.islower():
                allkeys[c] = (y, x)

            if c == '@':
                sy, sx = y, x

    seen = { (sy, sx, '') }

    clique = get_clique(d, height, width, sy, sx)
    clique_distances = []    

    for y, x in clique:
        clique_distances.append(measure_distances(d, height, width, clique, y, x))

    score = heuristic(d, clique, clique_distances, set(), sy, sx)

    cl = len(clique)
    passing_by = [[[] for _ in range(cl)] for _ in range(cl)]

    for i in range(cl - 1):
        ciy, cix = clique[i]
        for j in range(i + 1, cl):
            cjy, cjx = clique[j]
            between = get_between(d, height, width, ciy, cix, cjy, cjx)
            passing_by[i][j] = between
            passing_by[j][i] = between

    frontier = [[score, 0, sy, sx, set()]]

    while True:
        score, steps, y, x, keys = heappop(frontier)

        if score == steps:
            return steps

        ci = 0
        
        while clique[ci] != (y, x):
            ci += 1
        
        for cj in range(cl):
            if ci == cj:
                continue

            cjy, cjx = clique[cj]
            cell = d[cjy][cjx]

            if (cell.islower() and cell not in keys) or (cell.isupper() and cell.lower() in keys):
                passing = passing_by[ci][cj]
                passing_keys = [let for let in passing if let.islower()]
                passing_lowered_doors = [let.lower() for let in passing if let.isupper()]

                if set(passing_keys) - keys:
                    continue

                if set(passing_lowered_doors) - keys:
                    continue
                
                dkeys = set(keys)
                
                if cell.islower():
                    dkeys.add(cell)

                dsteps = clique_distances[ci][cj]
                dy, dx = clique[cj]

                tup = (dy, dx, ''.join(sorted(dkeys)))

                if tup not in seen:
                    seen.add(tup)
                    dscore = heuristic(d, clique, clique_distances, dkeys, dy, dx)
                    heappush(frontier, (dscore + steps + dsteps, steps + dsteps, dy, dx, dkeys))


def read_and_solve():
    with open('input_18.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
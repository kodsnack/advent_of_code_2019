import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def compress(y1, x1, y2, x2, y3, x3, y4, x4):
    return y1 + 100 * x1 + 10000 * y2 + 1000000 * x2 + 100000000 * y3 + 10000000000 * x3 + 1000000000000 * y4 + 100000000000000 * x4


def decompress(num):
    nums = []

    while num > 0:
        nums.append(num % 100)
        num //= 100

    return nums


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


def heuristic_clique(grid, clique, distances, keys, y, x):
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


def heuristic(grid, cliques, clique_distances, keys, y1, x1, y2, x2, y3, x3, y4, x4):
    return heuristic_clique(grid, cliques[0], clique_distances[0], keys, y1, x1) + heuristic_clique(grid, cliques[1], clique_distances[1], keys, y2, x2) + heuristic_clique(grid, cliques[2], clique_distances[2], keys, y3, x3) + heuristic_clique(grid, cliques[3], clique_distances[3], keys, y4, x4)


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
                
    compressed = (compress(sy - 1, sx - 1, sy - 1, sx + 1, sy + 1, sx + 1, sy + 1, sx - 1))

    seen = { (compressed, '') }

    cliques = [get_clique(d, height, width, sy - 1, sx - 1), get_clique(d, height, width, sy - 1, sx + 1), get_clique(d, height, width, sy + 1, sx + 1), get_clique(d, height, width, sy + 1, sx - 1)]
    clique_distances = []
    passing_by = []

    for ci in range(4):
        distances = []
        for pi, coords in enumerate(cliques[ci]):
            distances.append(measure_distances(d, height, width, cliques[ci], coords[0], coords[1]))			
        clique_distances.append(distances)

    score = heuristic(d, cliques, clique_distances, set(), sy - 1, sx - 1, sy - 1, sx + 1, sy + 1, sx + 1, sy + 1, sx - 1)

    for ci, clique in enumerate(cliques):
        cl = len(clique)
        passing = [[[] for _ in range(cl)] for _ in range(cl)]
        for i in range(cl - 1):
            ciy, cix = clique[i]
            for j in range(i + 1, cl):
                cjy, cjx = clique[j]
                between = get_between(d, height, width, ciy, cix, cjy, cjx)
                passing[i][j] = between
                passing[j][i] = between
        passing_by.append(passing)

    frontier = [[score, 0, compressed, set()]]

    while True:
        score, steps, compressed, keys = heappop(frontier)
        y1, x1, y2, x2, y3, x3, y4, x4 = decompress(compressed)

        if score == steps:
            return steps

        keysleft = [0, 0, 0, 0]
        for ci, clique in enumerate(cliques):
            for cy, cx in clique:
                cell = d[cy][cx]
                if cell.islower() and cell not in keys:
                    keysleft[ci] += 1

        if keysleft[0] > 0:
            ci = 0
            
            while cliques[0][ci] != (y1, x1):
                ci += 1
            
            for cj in range(len(cliques[0])):
                if ci == cj:
                    continue

                cjy, cjx = cliques[0][cj]
                cell = d[cjy][cjx]

                if (cell.islower() and cell not in keys) or (cell.isupper() and cell.lower() in keys):
                    passing = passing_by[0][ci][cj]
                    passing_keys = [let for let in passing if let.islower()]
                    passing_lowered_doors = [let.lower() for let in passing if let.isupper()]

                    if set(passing_keys) - keys:
                        continue

                    if set(passing_lowered_doors) - keys:
                        continue
                    
                    dkeys = set(keys)
                    
                    if cell.islower():
                        dkeys.add(cell)

                    dsteps = clique_distances[0][ci][cj]
                    dy, dx = cliques[0][cj]
                    dcompressed = compress(dy, dx, y2, x2, y3, x3, y4, x4)

                    tup = (dcompressed, ''.join(sorted(dkeys)))

                    if tup not in seen:
                        seen.add(tup)
                        dscore = heuristic(d, cliques, clique_distances, dkeys, dy, dx, y2, x2, y3, x3, y4, x4)
                        heappush(frontier, (dscore + steps + dsteps, steps + dsteps, dcompressed, dkeys))
        if keysleft[1] > 0:
            ci = 0
            
            while cliques[1][ci] != (y2, x2):
                ci += 1
            
            for cj in range(len(cliques[1])):
                if ci == cj:
                    continue

                cjy, cjx = cliques[1][cj]
                cell = d[cjy][cjx]

                if (cell.islower() and cell not in keys) or (cell.isupper() and cell.lower() in keys):
                    passing = passing_by[1][ci][cj]
                    passing_keys = [let for let in passing if let.islower()]
                    passing_lowered_doors = [let.lower() for let in passing if let.isupper()]

                    if set(passing_keys) - keys:
                        continue

                    if set(passing_lowered_doors) - keys:
                        continue
                    
                    dkeys = set(keys)
                    
                    if cell.islower():
                        dkeys.add(cell)

                    dsteps = clique_distances[1][ci][cj]
                    dy, dx = cliques[1][cj]
                    dcompressed = compress(y1, x1, dy, dx, y3, x3, y4, x4)

                    tup = (dcompressed, ''.join(sorted(dkeys)))

                    if tup not in seen:
                        seen.add(tup)
                        dscore = heuristic(d, cliques, clique_distances, dkeys, y1, x1, dy, dx, y3, x3, y4, x4)
                        heappush(frontier, (dscore + steps + dsteps, steps + dsteps, dcompressed, dkeys))
        if keysleft[2] > 0:
            ci = 0
            
            while cliques[2][ci] != (y3, x3):
                ci += 1
            
            for cj in range(len(cliques[2])):
                if ci == cj:
                    continue

                cjy, cjx = cliques[2][cj]
                cell = d[cjy][cjx]

                if (cell.islower() and cell not in keys) or (cell.isupper() and cell.lower() in keys):
                    passing = passing_by[2][ci][cj]
                    passing_keys = [let for let in passing if let.islower()]
                    passing_lowered_doors = [let.lower() for let in passing if let.isupper()]

                    if set(passing_keys) - keys:
                        continue

                    if set(passing_lowered_doors) - keys:
                        continue
                    
                    dkeys = set(keys)
                    
                    if cell.islower():
                        dkeys.add(cell)

                    dsteps = clique_distances[2][ci][cj]
                    dy, dx = cliques[2][cj]
                    dcompressed = compress(y1, x1, y2, x2, dy, dx, y4, x4)

                    tup = (dcompressed, ''.join(sorted(dkeys)))

                    if tup not in seen:
                        seen.add(tup)
                        dscore = heuristic(d, cliques, clique_distances, dkeys, y1, x1, y2, x2, dy, dx, y4, x4)
                        heappush(frontier, (dscore + steps + dsteps, steps + dsteps, dcompressed, dkeys))
        if keysleft[3] > 0:
            ci = 0
            
            while cliques[3][ci] != (y4, x4):
                ci += 1
            
            for cj in range(len(cliques[3])):
                if ci == cj:
                    continue

                cjy, cjx = cliques[3][cj]
                cell = d[cjy][cjx]

                if (cell.islower() and cell not in keys) or (cell.isupper() and cell.lower() in keys):
                    passing = passing_by[3][ci][cj]
                    passing_keys = [let for let in passing if let.islower()]
                    passing_lowered_doors = [let.lower() for let in passing if let.isupper()]

                    if set(passing_keys) - keys:
                        continue

                    if set(passing_lowered_doors) - keys:
                        continue
                    
                    dkeys = set(keys)
                    
                    if cell.islower():
                        dkeys.add(cell)

                    dsteps = clique_distances[3][ci][cj]
                    dy, dx = cliques[3][cj]
                    dcompressed = compress(y1, x1, y2, x2, y3, x3, dy, dx)

                    tup = (dcompressed, ''.join(sorted(dkeys)))

                    if tup not in seen:
                        seen.add(tup)
                        dscore = heuristic(d, cliques, clique_distances, dkeys, y1, x1, y2, x2, y3, x3, dy, dx)
                        heappush(frontier, (dscore + steps + dsteps, steps + dsteps, dcompressed, dkeys))
    

def read_and_solve():
    with open('input_18.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def find_portals(data, height, width):
    start, end = (-1, -1), (-1, -1)
    labels = defaultdict(list)
    portals = {}

    for y in range(height):
        for x in range(width):
            if data[y][x] != '.':
                continue

            label = ''
            center = 3 < y < height - 3 and 3 < x < width - 3
            direction = 1 if center else -1

            for ny, nx in helpers.get_moves(height, width, y, x):
                if data[ny][nx].isalpha():
                    if data[ny - 1][nx].isalpha():
                        label = data[ny - 1][nx] + data[ny][nx]
                    elif data[ny + 1][nx].isalpha():
                        label = data[ny][nx] + data[ny + 1][nx]
                    elif data[ny][nx - 1].isalpha():
                        label = data[ny][nx - 1] + data[ny][nx]
                    elif data[ny][nx + 1].isalpha():
                        label = data[ny][nx] + data[ny][nx + 1]
                    break

            if label:
                labels[label].append((y, x, direction))

    for label, locations in labels.items():
        if label == 'AA':
            start = (locations[0][0], locations[0][1])
            continue
        elif label == 'ZZ':
            end = (locations[0][0], locations[0][1])
            continue

        l1y, l1x, l1d = locations[0]
        l2y, l2x, l2d = locations[1]
        
        portals[(l1y, l1x)] = (l2y, l2x, l1d)
        portals[(l2y, l2x)] = (l1y, l1x, l2d)


    return portals, start, end


def get_distances(grid, height, width, locations, sy, sx):
    seen = { (sy, sx) }
    distances = {}
    frontier = [[0, sy, sx]]

    for distance, y, x in frontier:
        if (y, x) in locations:
            distances[(y, x)] = distance

        for ny, nx in helpers.get_moves(height, width, y, x):
            if (ny, nx) not in seen and grid[ny][nx] == '.':
                seen.add((ny, nx))
                frontier.append([distance + 1, ny, nx])

    del distances[(sy, sx)]
    
    return distances


def solve(data):
    height = len(data)
    width = len(data[0])
    narrower_width = 0

    for y in range(height):
        for w in range(len(data[y]) - 1, -1, -1):
            if data[y][w] == '#' or data[y][w].isalpha():
                narrower_width = max(narrower_width, w)
                break

    width = narrower_width
    
    portals, start, end = find_portals(data, height, width)
    all_locations = { start, end } | portals.keys()

    distances = {}

    for ly, lx in all_locations:
        distances[(ly, lx)] = get_distances(data, height, width, all_locations, ly, lx)

    frontier = [[0, start[0], start[1], 0]]
    seen = { (start[0], start[1], 0): 0 }

    while True:
        distance, y, x, level = heappop(frontier)

        if (y, x) == end and level == 0:
            return distance

        if (y, x) in portals:
            ny, nx, direction = portals[(y, x)]
            if level > 0 or direction ==  1:
                if (ny, nx, level + direction) not in seen or seen[(ny, nx, level + direction)] > distance + 1:
                    seen[(ny, nx, level + direction)] = distance + 1
                    heappush(frontier, [distance + 1, ny, nx, level + direction])

        for coords, d in distances[(y, x)].items():
            if (coords[0], coords[1], level) in seen and seen[(coords[0], coords[1], level)] <= distance + d:
                continue

            if ((coords[0], coords[1]) == end or (coords[0], coords[1]) == start) and level != 0:
                continue

            seen[(coords[0], coords[1], level)] = distance + d
            heappush(frontier, [distance + d, coords[0], coords[1], level])
    

def read_and_solve():
    with open('input_20.txt') as f:
        data = [line for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
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
                labels[label].append((y, x))

    for label, locations in labels.items():
        if label == 'AA':
            start = locations[0]
            continue
        elif label == 'ZZ':
            end = locations[0]
            continue

        portals[locations[0]] = locations[1]
        portals[locations[1]] = locations[0]


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

    frontier = [[0, start[0], start[1]]]
    seen = { (start[0], start[1]): 0 }

    while True:
        distance, y, x = heappop(frontier)

        if (y, x) == end:
            return distance

        if (y, x) in portals:
            ny, nx = portals[(y, x)]
            if (ny, nx) not in seen or seen[(ny, nx)] > distance + 1:
                seen[(ny, nx)] = distance + 1
                heappush(frontier, [distance + 1, ny, nx])

        for coords, d in distances[(y, x)].items():
            if coords in seen and seen[(coords[0], coords[1])] <= distance + d:
                continue

            seen[coords] = distance + d
            heappush(frontier, [distance + d, coords[0], coords[1]])
    

def read_and_solve():
    with open('input_20.txt') as f:
        data = [line for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
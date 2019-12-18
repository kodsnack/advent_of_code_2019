import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def heuristic(grid, keys, allkeys, y, x):
    dist = 0

    for key in allkeys.keys() - keys:
        dist = max(dist, helpers.manhattan((y, x), allkeys[key]))

    return dist


def solve(d):
    height = len(d)
    width = len(d[0])

    py, px = -1, -1
    allkeys = {}

    for y in range(height):
        for x in range(width):
            c = d[y][x]

            if c == '@':
                py, px = y, x
            if c.isalpha() and c.islower():
                allkeys[c] = (y, x)

    h = heuristic(d, set(), allkeys, py, px)
    frontier = [[h, 0, py, px, set()]]
    seen = { (y, x, str(set())) }
    largest = 0

    while True:
        score, steps, y, x, keys = heappop(frontier)

        if len(keys) == len(allkeys):
            return steps

        if steps > largest:
            largest = steps
            print(largest, len(keys), len(allkeys))

        if y > 0 and d[y - 1][x] != '#':
            c = d[y - 1][x]
            if c in '.@':
                newkeys = set(keys)
                tup = (y - 1, x, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y - 1, x)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y - 1, x, newkeys))
            elif c.islower():
                newkeys = set(keys)
                newkeys.add(c)
                tup = (y - 1, x, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y - 1, x)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y - 1, x, newkeys))
            elif c.isupper() and c.lower() in keys:
                newkeys = set(keys)
                tup = (y - 1, x, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y - 1, x)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y - 1, x, newkeys))
        if y < height - 1 and d[y + 1][x] != '#':
            c = d[y + 1][x]
            if c in '.@':
                newkeys = set(keys)
                tup = (y + 1, x, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y + 1, x)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y + 1, x, newkeys))
            elif c.islower():
                newkeys = set(keys)
                newkeys.add(c)
                tup = (y + 1, x, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y + 1, x)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y + 1, x, newkeys))
            elif c.isupper() and c.lower() in keys:
                newkeys = set(keys)
                tup = (y + 1, x, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y + 1, x)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y + 1, x, newkeys))
        
        if x > 0 and d[y][x - 1] != '#':
            c = d[y][x - 1]
            if c in '.@':
                newkeys = set(keys)
                tup = (y, x - 1, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y, x - 1)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y, x - 1, newkeys))
            elif c.islower():
                newkeys = set(keys)
                newkeys.add(c)
                tup = (y, x - 1, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y, x - 1)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y, x - 1, newkeys))
            elif c.isupper() and c.lower() in keys:
                newkeys = set(keys)
                tup = (y, x - 1, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y, x - 1)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y, x - 1, newkeys))
        if x > 0 and d[y][x + 1] != '#':
            c = d[y][x + 1]
            if c in '.@':
                newkeys = set(keys)
                tup = (y, x + 1, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y, x + 1)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y, x + 1, newkeys))
            elif c.islower():
                newkeys = set(keys)
                newkeys.add(c)
                tup = (y, x + 1, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y, x + 1)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y, x + 1, newkeys))
            elif c.isupper() and c.lower() in keys:
                newkeys = set(keys)
                tup = (y, x + 1, str(newkeys))
                if tup not in seen:
                    score = heuristic(d, newkeys, allkeys, y, x + 1)
                    seen.add(tup)
                    heappush(frontier, (score + steps + 1, steps + 1, y, x + 1, newkeys)) 
    

def read_and_solve():
    with open('input_18.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
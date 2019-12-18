import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(d):
    height = len(d)
    width = len(d[0])

    py, px = -1, -1
    allkeys = set()

    for y in range(height):
        for x in range(width):
            c = d[y][x]

            if c == '@':
                py, px = y, x
            if c.isalpha() and c.islower():
                allkeys.add(c)

    frontier = [[0, py, px, '']]
    seen = { (y, x, '') }
    largest = 0

    for steps, y, x, keys in frontier:
        if len(keys) == len(allkeys):
            return steps

        if steps > largest:
            largest = steps
            print(largest)

        if y > 0 and d[y - 1][x] != '#':
            c = d[y - 1][x]
            if c in '.@':
                tup = (y - 1, x, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y - 1, x, keys])
            elif c.islower():
                newkeys = str(keys)
                if c not in newkeys:
                    newkeys += c
                tup = (y - 1, x, newkeys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y - 1, x, newkeys])
            elif c.isupper() and c.lower() in keys:
                tup = (y - 1, x, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y - 1, x, keys])
        if y < height - 1 and d[y + 1][x] != '#':
            c = d[y + 1][x]
            if c in '.@':
                tup = (y + 1, x, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y + 1, x, keys])
            elif c.islower():
                newkeys = str(keys)
                if c not in newkeys:
                    newkeys += c
                tup = (y + 1, x, newkeys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y + 1, x, newkeys])
            elif c.isupper() and c.lower() in keys:
                tup = (y + 1, x, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y + 1, x, keys])
        
        if x > 0 and d[y][x - 1] != '#':
            c = d[y][x - 1]
            if c in '.@':
                tup = (y, x - 1, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y, x - 1, keys])
            elif c.islower():
                newkeys = str(keys)
                if c not in newkeys:
                    newkeys += c
                tup = (y, x - 1, newkeys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y, x - 1, newkeys])
            elif c.isupper() and c.lower() in keys:
                tup = (y, x - 1, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y, x - 1, keys])
        if x < width - 1 and d[y][x + 1] != '#':
            c = d[y][x + 1]
            if c in '.@':
                tup = (y, x + 1, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y, x + 1, keys])
            elif c.islower():
                newkeys = str(keys)
                if c not in newkeys:
                    newkeys += c
                tup = (y, x + 1, newkeys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y, x + 1, newkeys])
            elif c.isupper() and c.lower() in keys:
                tup = (y, x + 1, keys)
                if tup not in seen:
                    seen.add(tup)
                    frontier.append([steps + 1, y, x + 1, keys])    
    

def read_and_solve():
    with open('input_18.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
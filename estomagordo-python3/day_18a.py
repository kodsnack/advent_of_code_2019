import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def shortest(grid, height, width, y1, x1, y2, x2):
    frontier = [[0, y1, x1, '']]
    seen = set()

    for steps, y, x, doors in frontier:
        if y == y2 and x == x2:
            return steps, doors

        steps += 1

        if y > 0:
            c = grid[y - 1][x]
            if c != '#' and (y - 1, x) not in seen:
                seen.add((y - 1, x))
                newdoors = str(doors)
                if c.isupper() and c not in newdoors:
                    newdoors += c
                frontier.append([steps, y - 1, x, newdoors])
        if y < height - 1:
            c = grid[y + 1][x]
            if c != '#' and (y + 1, x) not in seen:
                seen.add((y + 1, x))
                newdoors = str(doors)
                if c.isupper() and c not in newdoors:
                    newdoors += c
                frontier.append([steps, y + 1, x, newdoors])
        if x > 0:
            c = grid[y][x - 1]
            if c != '#' and (y, x - 1) not in seen:
                seen.add((y, x - 1))
                newdoors = str(doors)
                if c.isupper() and c not in newdoors:
                    newdoors += c
                frontier.append([steps, y, x - 1, newdoors])
        if x < width - 1:
            c = grid[y][x + 1]
            if c != '#' and (y, x + 1) not in seen:
                seen.add((y, x + 1))
                newdoors = str(doors)
                if c.isupper() and c not in newdoors:
                    newdoors += c
                frontier.append([steps, y, x + 1, newdoors])


def heuristic(grid, keys, allkeys, y, x):
    dist = 0

    for key in allkeys.keys() - keys:
        dist = max(dist, helpers.manhattan((y, x), allkeys[key]))

    return dist


def solve(d):
    height = len(d)
    width = len(d[0])

    points = []
    allkeys = set()

    for y in range(height):
        for x in range(width):
            c = d[y][x]

            if c not in '.#' and not c.isupper():
                points.append([y, x, c])

            if c.islower():
                allkeys.add(c)

    distances = [[[0, ''] for _ in range(len(points))] for _ in range(len(points))]

    for i in range(len(points) - 1):
        print(i)
        y1, x1 = points[i][:2]
        for j in range(i + 1, len(points)):
            y2, x2 = points[j][:2]
            dist, doors = shortest(d, height, width, y1, x1, y2, x2)
            distances[i][j] = distances[j][i] = [dist, doors]

    pos = 0
    while points[pos][2] != '@':
        pos += 1

    frontier = [[0, pos, '']]

    best = 0

    while True:
        steps, pos, keys = heappop(frontier)

        if len(keys) > best:
            best = len(keys)
            print(best)

        if len(keys) == len(allkeys):
            return steps

        for i in range(len(points)):
            if i == pos:
                continue

            distance, doors = distances[pos][i]
            ky, kx, key = points[i]

            if key in keys:
                continue

            if any(c.lower() not in keys for c in doors):
                continue

            newkeys = str(keys)
            newkeys += key

            heappush(frontier, [steps + distance, i, newkeys])



    # h = heuristic(d, set(), allkeys, py, px)
    # frontier = [[h, 0, py, px, set()]]
    # seen = { (y, x, str(set())) }
    # largest = 0

    # while True:
    #     score, steps, y, x, keys = heappop(frontier)

    #     if len(keys) == len(allkeys):
    #         return steps

    #     if steps > largest:
    #         largest = steps
    #         print(largest, len(keys), len(allkeys))

    #     if y > 0 and d[y - 1][x] != '#':
    #         c = d[y - 1][x]
    #         if c in '.@':
    #             newkeys = set(keys)
    #             tup = (y - 1, x, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y - 1, x)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y - 1, x, newkeys))
    #         elif c.islower():
    #             newkeys = set(keys)
    #             newkeys.add(c)
    #             tup = (y - 1, x, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y - 1, x)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y - 1, x, newkeys))
    #         elif c.isupper() and c.lower() in keys:
    #             newkeys = set(keys)
    #             tup = (y - 1, x, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y - 1, x)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y - 1, x, newkeys))
    #     if y < height - 1 and d[y + 1][x] != '#':
    #         c = d[y + 1][x]
    #         if c in '.@':
    #             newkeys = set(keys)
    #             tup = (y + 1, x, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y + 1, x)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y + 1, x, newkeys))
    #         elif c.islower():
    #             newkeys = set(keys)
    #             newkeys.add(c)
    #             tup = (y + 1, x, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y + 1, x)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y + 1, x, newkeys))
    #         elif c.isupper() and c.lower() in keys:
    #             newkeys = set(keys)
    #             tup = (y + 1, x, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y + 1, x)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y + 1, x, newkeys))
        
    #     if x > 0 and d[y][x - 1] != '#':
    #         c = d[y][x - 1]
    #         if c in '.@':
    #             newkeys = set(keys)
    #             tup = (y, x - 1, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y, x - 1)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y, x - 1, newkeys))
    #         elif c.islower():
    #             newkeys = set(keys)
    #             newkeys.add(c)
    #             tup = (y, x - 1, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y, x - 1)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y, x - 1, newkeys))
    #         elif c.isupper() and c.lower() in keys:
    #             newkeys = set(keys)
    #             tup = (y, x - 1, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y, x - 1)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y, x - 1, newkeys))
    #     if x > 0 and d[y][x + 1] != '#':
    #         c = d[y][x + 1]
    #         if c in '.@':
    #             newkeys = set(keys)
    #             tup = (y, x + 1, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y, x + 1)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y, x + 1, newkeys))
    #         elif c.islower():
    #             newkeys = set(keys)
    #             newkeys.add(c)
    #             tup = (y, x + 1, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y, x + 1)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y, x + 1, newkeys))
    #         elif c.isupper() and c.lower() in keys:
    #             newkeys = set(keys)
    #             tup = (y, x + 1, str(newkeys))
    #             if tup not in seen:
    #                 score = heuristic(d, newkeys, allkeys, y, x + 1)
    #                 seen.add(tup)
    #                 heappush(frontier, (score + steps + 1, steps + 1, y, x + 1, newkeys)) 
    

def read_and_solve():
    with open('input_18.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
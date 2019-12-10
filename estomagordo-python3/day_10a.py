import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def reduce(x, y):
    for fac in range(2, min(abs(x), abs(y)) + 1):
        if fac > max(abs(x), abs(y)):
            return x, y

        while x % fac == 0 and y % fac == 0:
            x //= fac
            y //= fac
        
    return x, y


def solve(d):
    asteroids = []

    for y, row in enumerate(d):
        for x, c in enumerate(row):
            if c == '#':
                asteroids.append((x, y))

    best = (0, 0, 0)

    for i, asteroid in enumerate(asteroids):
        ax, ay = asteroid
        angles = defaultdict(list)
        for j, other in enumerate(asteroids):
            if i == j:
                continue

            ox, oy = other
            xdiff = ox - ax
            ydiff = oy - ay
            if ydiff == 0:
                ydiff = 1
                xdiff = 10**12 if ox > ax else -10**12

            xdiff, ydiff = reduce(xdiff, ydiff)
            if xdiff == 0:
                ydiff = 1 if ydiff > 0 else -1

            angles[(xdiff, ydiff)].append((ox, oy))

        score = len(angles)

        best = max(best, (score, ax, ay))

    return best[0]

def read_and_solve():
    with open('input_10.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
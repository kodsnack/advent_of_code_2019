import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from fractions import Fraction


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
    bestangles = None

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

        # if (10**12, 1) in angles:
        #     if any(ast[0] < ax for ast in angles[(10**12, 1)]) and any(ast[0] > ax for ast in angles[(10**12, 1)]):
        #         score += 1

        # if (0, 1) in angles:
        #     if any(ast[1] < ay for ast in angles[(0, 1)]) and any(ast[1] > ax for ast in angles[(0, 1)]):
        #         score += 1

        if (score, ax, ay) > best:
            best = (score, ax, ay)
            bestangles = angles
            
    ax, ay = best[1:]

    new_order = []

    types = [[] for _ in range(8)]

    for angle, asts in bestangles.items():
        typescore = 0

        dx, dy = angle

        if dx > 0 and dy < 0:
            if dx < 10*12:
                typescore = 1
            else:
                typescore = 2
        elif dx > 0:
            typescore = 3
        elif dx == 0 and dy > 0:
            typescore = 4
        elif dx < 0 and dy > 0:
            if dx > -10**12:
                typescore = 5
            else:
                typescore = 6
        elif dx < 0 and dy < 0:
            typescore = 7
        
        types[typescore].append((angle, asts))

    if types[0]:
        new_order.append((0, types[0][0][0]))

    if types[1]:
        types[1].sort(key=lambda pair: pair[0][0] / pair[0][1])
        for angle, asts in types[1]:
            asts.sort(key=lambda ast: -ast[1])
            new_order.append((1, asts[0]))

    if types[2]:
        new_order.append((2, types[2][0][0]))

    if types[3]:
        types[3].sort(key=lambda pair: -(pair[0][0] / pair[0][1]))
        for angle, asts in types[3]:
            asts.sort(key=lambda ast: ast[1])
            new_order.append((3, asts[0]))

    if types[4]:
        new_order.append((4, types[4][0][0]))

    if types[5]:
        types[5].sort(key=lambda pair: -(pair[0][0] / pair[0][1]))
        for angle, asts in types[5]:
            asts.sort(key=lambda ast: ast[1])
            new_order.append((5, asts[0]))

    if types[6]:
        new_order.append((6, types[4][0][0]))

    if types[7]:
        types[7].sort(key=lambda pair: -(pair[0][0] / pair[0][1]))
        for angle, asts in types[7]:
            asts.sort(key=lambda ast: -ast[1])
            new_order.append((7, asts[0]))

    # new_order.sort()

    return new_order[199][1][0] * 100 + new_order[199][1][1]

def read_and_solve():
    with open('input_10.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
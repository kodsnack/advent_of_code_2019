import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations


def paint(painted):
    loy = 1000
    hiy = -1000
    lox = 1000
    hix = -1000

    out = []

    for y, x in painted.keys():
        loy = min(loy, y)
        hiy = max(hiy, y)
        lox = min(lox, x)
        hix = max(hix, x)

    for y in range(loy, hiy + 1):
        row = []
        for x in range(lox, hix + 1):
            row.append('.' if (y, x) not in painted or painted[(y, x)] == 0 else '#')
        out.append(''.join(row))

    return '\n'.join(out)

def solve(data, inp):
    d = defaultdict(int)

    for i, v in enumerate(data):
        d[i] = v

    painted = {}
    y = 0
    x = 0
    directions = ((-1, 0), (0, 1), (1, 0), (0, -1))
    facing = 0

    p = 0
    relbase = 0
    steps = 0
    output_count = 0
    first = True

    while True:
        steps += 1
        amode = (d[p] % 1000) // 100
        bmode = (d[p] % 10000) // 1000
        cmode = (d[p] % 100000) // 10000

        a = d[p + 1] if amode == 1 else d[d[p + 1]] if amode == 0 else d[d[p + 1] + relbase]
        b = d[p + 2] if bmode == 1 else d[d[p + 2]] if bmode == 0 else d[d[p + 2] + relbase]
        c = d[p + 3] if cmode == 1 else d[d[p + 3]] if cmode == 0 else d[d[p + 3] + relbase]

        if d[p] % 100 == 99:
            return paint(painted)
        elif d[p] % 100 == 1:
            if cmode == 0:
                d[d[p + 3]] = a + b
            else:
                d[d[p + 3] + relbase] = a + b
            p += 4
        elif d[p] % 100 == 2:
            if cmode == 0:
                d[d[p + 3]] = a * b
            else:
                d[d[p + 3] + relbase] = a * b
            p += 4
        elif d[p] % 100 == 3:
            if amode == 0:
                d[d[p + 1]] = inp if first else 0 if not (y, x) in painted else painted[(y, x)]
            else:
                d[d[p + 1] + relbase] = inp if first else 0 if not (y, x) in painted else painted[(y, x)]
            first = False
            p += 2
        elif d[p] % 100 == 4:
            if output_count % 2 == 0:
                painted[(y, x)] = a
            else:
                if a == 0:
                    facing = (facing - 1) % 4
                else:
                    facing = (facing + 1) % 4

                y += directions[facing][0]
                x += directions[facing][1]
            output_count += 1
            p += 2
        elif d[p] % 100 == 5:
            if a != 0:
                p = b
            else:
                p += 3
        elif d[p] % 100 == 6:
            if a == 0:
                p = b
            else:
                p += 3
        elif d[p] % 100 == 7:
            cc = 1 if a < b else 0
            if cmode == 0:
                d[d[p + 3]] = cc
            else:
                d[d[p + 3] + relbase] = cc
            p += 4
        elif d[p] % 100 == 8:
            cc = 1 if a == b else 0
            if cmode == 0:
                d[d[p + 3]] = cc
            else:
                d[d[p + 3] + relbase] = cc
            p += 4
        elif d[p] % 100 == 9:
            relbase += a
            p += 2
        else:
            print('uh oh', d[p])

    return 'outside-loop'


def read_and_solve():
    with open('input_11.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 1)

if __name__ == '__main__':
    print(read_and_solve())
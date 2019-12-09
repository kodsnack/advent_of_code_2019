import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations


def solve(data, inp):
    # data = [209,1,203,-1,4,0,99]
    # inp = 20
    d = defaultdict(int)

    for i, v in enumerate(data):
        d[i] = v

    p = 0
    relbase = 0
    steps = 0

    while True:
        steps += 1
        amode = (d[p] % 1000) // 100
        bmode = (d[p] % 10000) // 1000
        cmode = (d[p] % 100000) // 10000

        a = d[p + 1] if amode == 1 else d[d[p + 1]] if amode == 0 else d[d[p + 1] + relbase]
        b = d[p + 2] if bmode == 1 else d[d[p + 2]] if bmode == 0 else d[d[p + 2] + relbase]
        c = d[p + 3] if cmode == 1 else d[d[p + 3]] if cmode == 0 else d[d[p + 3] + relbase]

        if d[p] % 100 == 99:
            return 'ninety-nine'
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
                d[d[p + 1]] = inp
            else:
                d[d[p + 1] + relbase] = inp
            p += 2
        elif d[p] % 100 == 4:
            return a
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
    with open('input_9.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 1)

if __name__ == '__main__':
    print(read_and_solve())
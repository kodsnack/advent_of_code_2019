import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations

def first(d, inp):
    p = 0

    while p < len(d):
        a = -1 if p > len(d) - 2 else d[p + 1] if ((d[p] % 1000) // 100) == 1 else d[d[p + 1]]
        b = -1 if p > len(d) - 3 else d[p + 2] if ((d[p] % 10000) // 1000) == 1 else -1 if d[p + 2] >= len(d) else d[d[p + 2]]

        if d[p] % 100 == 1:
            d[d[p + 3]] = a + b
            p += 4
        elif d[p] % 100 == 2:
            d[d[p + 3]] = a * b
            p += 4
        elif d[p] % 100 == 3:
            d[d[p + 1]] = inp
            p += 2
        elif d[p] % 100 == 4:
            if a != 0:
                return d
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
            c = 1 if a < b else 0
            d[d[p + 3]] = c
            p += 4
        elif d[p] % 100 == 8:
            c = 1 if a == b else 0
            d[d[p + 3]] = c
            p += 4


def solve_amp(d, inp_a, inp_b):
    # d = first(d, inp_a)
    inp = [inp_a, inp_b]
    inpcount = 0
    p = 0

    while p < len(d):
        a = -1 if p > len(d) - 2 else d[p + 1] if ((d[p] % 1000) // 100) == 1 else d[d[p + 1]]
        b = -1 if p > len(d) - 3 else d[p + 2] if ((d[p] % 10000) // 1000) == 1 else -1 if d[p + 2] >= len(d) else d[d[p + 2]]

        if d[p] % 100 == 99:
            return d[0]
        if d[p] % 100 == 1:
            d[d[p + 3]] = a + b
            p += 4
        elif d[p] % 100 == 2:
            d[d[p + 3]] = a * b
            p += 4
        elif d[p] % 100 == 3:
            d[d[p + 1]] = inp[inpcount]
            inpcount = 1
            p += 2
        elif d[p] % 100 == 4:
            if a != 0:
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
            c = 1 if a < b else 0
            d[d[p + 3]] = c
            p += 4
        elif d[p] % 100 == 8:
            c = 1 if a == b else 0
            d[d[p + 3]] = c
            p += 4


def solve(d):
    best = 0

    for p in permutations(range(5)):
        val = solve_amp(list(d), p[0], 0)
        print(val)

        for x in range(1, 5):
            val = solve_amp(list(d), p[x], val)
            print(val)

        best = max(best, val)

    return best


def read_and_solve():
    with open('input_7.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
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


def solve_amp(d, p, inp_a, inp_b, inpcount):
    # d = first(d, inp_a)
    inp = [inp_a, inp_b]
    # inpcount = 0

    while p < len(d):
        a = -1 if p > len(d) - 2 else d[p + 1] if ((d[p] % 1000) // 100) == 1 else d[d[p + 1]]
        b = -1 if p > len(d) - 3 else d[p + 2] if ((d[p] % 10000) // 1000) == 1 else -1 if d[p + 2] >= len(d) else d[d[p + 2]]

        if d[p] % 100 == 99:
            return d[0], d, 0, True
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
                return a, d, p + 2, False
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

    for perm in permutations(range(5, 10)):
        ds = [list(d) for _ in range(5)]
        poss = [0 for _ in range(5)]
        val = 0

        pos = 0
        prev = 0
        
        while True:
            val, dp, p, finished = solve_amp(ds[pos % 5], poss[pos % 5], perm[pos % 5], val, 0 if pos < 5 else 1)
            ds[pos % 5] = dp
            poss[pos % 5] = p
            # print(val, finished)
            # if val > 18000:
            #     print(val, pos % 5)
            if finished:
                # print(prev, pos % 5)
                best = max(best, prev)
                break
            prev = val
            pos += 1

    return best


def read_and_solve():
    with open('input_7.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
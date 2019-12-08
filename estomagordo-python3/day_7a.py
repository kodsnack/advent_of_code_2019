import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations


def solve_amp(d, p, inputs):
    inp_pos = 0

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
            d[d[p + 1]] = inputs[inp_pos]
            inp_pos = 1
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


def solve(d, lo, hi, ampcount):
    best = 0

    for perm in permutations(range(lo, hi)):
        ds = [list(d) for _ in range(ampcount)]
        pointers = [0 for _ in range(ampcount)]
        val = 0

        pos = 0
        prev = 0
        
        while True:
            data = ds[pos % ampcount]
            pointer = pointers[pos % ampcount]
            inputs = (perm[pos] if pos < ampcount else val, val)
            val, dp, p, finished = solve_amp(data, pointer, inputs)

            ds[pos % ampcount] = dp
            pointers[pos % ampcount] = p
            if finished:
                best = max(best, prev)
                break
            prev = val
            pos += 1

    return best


def read_and_solve():
    with open('input_7.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 0, 5, 5)

if __name__ == '__main__':
    print(read_and_solve())
import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(d):
    lo, hi = d
    met = 0

    for x in range(lo, hi + 1):
        s = str(x)
        adj = False
        nondec = True

        for i, c in enumerate(s):
            if i == 0:
                continue

            if c == s[i - 1]:
                adj = True              

            if int(c) < int(s[i - 1]):
                nondec = False
                break

        if adj and nondec:
            met += 1

    return met
    

def read_and_solve():
    with open('input_4.txt') as f:
        data = list(map(int, f.readline().split('-')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
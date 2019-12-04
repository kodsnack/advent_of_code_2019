import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(d):
    lo, hi = d
    met = 0

    for x in range(lo, hi + 1):
        s = str(x)

        trueadj = False
        nondec = True
        
        for i, c in enumerate(s):
            if i == 0:
                continue

            if int(c) < int(s[i - 1]):
                nondec = False
                break

            if len(set(s[i-1:i+1])) == 1 and (i == len(s) - 1 or s[i + 1] != c) and (i == 1 or s[i - 2] != c):
                trueadj = True

        if trueadj and nondec:
            met += 1

    return met
    

def read_and_solve():
    with open('input_4.txt') as f:
        data = list(map(int, f.readline().split('-')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
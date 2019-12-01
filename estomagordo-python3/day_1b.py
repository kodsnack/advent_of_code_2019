import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def calc(n):
    return max(0, n // 3 - 2)


def rec_calc(n):
    val = calc(n)

    if val > 0:
        return val + rec_calc(val)

    return 0


def solve(d):
    return sum(rec_calc(n) for n in d)


def read_and_solve():
    with open('input_1.txt') as f:
        data = [int(line.rstrip()) for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
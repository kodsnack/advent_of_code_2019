import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(d):
    pass


def read_and_solve():
    with open('input_3.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
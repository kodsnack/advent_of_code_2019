import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations
from intcode import Computer


def solve(data, inp):
    computer = Computer(data, inp)
    return computer.get_output()[1]


def read_and_solve():
    with open('input_9.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 2)

if __name__ == '__main__':
    print(read_and_solve())
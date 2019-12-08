import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(d, width, height):
    layers_by_zero_count = defaultdict(list)
    layer = Counter()

    for i, c in enumerate(d):
        layer[c] += 1

        if i % (width * height) == (width * height) - 1:
            layers_by_zero_count[layer['0']].append(layer)
            layer = Counter()

    sought = layers_by_zero_count[min(layers_by_zero_count.keys())][0]
    return sought['1'] * sought['2']


def read_and_solve():
    with open('input_8.txt') as f:
        data = f.readline().rstrip()
        return solve(data, 25, 6)

if __name__ == '__main__':
    print(read_and_solve())
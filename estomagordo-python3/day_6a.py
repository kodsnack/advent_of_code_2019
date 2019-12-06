import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(d):
    graph = {}

    for line in d:
        a, b = line.split(')')
        graph[b] = a
        if a not in graph:
            graph[a] = 'STOP'

    vals = graph.values()

    count = 0

    for node in graph.keys():
        # if node in vals:
        #     continue

        orig = node

        runlen = -1

        while node in graph:
            runlen += 1
            node = graph[node]

        count += runlen

    return count

def read_and_solve():
    with open('input_6.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
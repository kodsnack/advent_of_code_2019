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

    youorbits = {}
    sanorbits = {}

    for node in graph.keys():
        mode = 0 if node == graph['YOU'] else 1 if node == graph['SAN'] else -1

        if mode == -1:
            continue

        runlen = 0

        while node in graph:
            runlen += 1
            node = graph[node]
            if mode == 0:
                youorbits[node] = runlen
            else:
                sanorbits[node] = runlen

    best = 10**10

    for k, v in youorbits.items():
        if k not in sanorbits:
            continue

        best = min(best, v + sanorbits[k])

    return best

    return count

def read_and_solve():
    with open('input_6.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
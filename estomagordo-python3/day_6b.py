import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def pathize(graph, node):
    path = {}
    runlen = 0

    while node in graph:
        runlen += 1
        node = graph[node]
        path[node] = runlen

    return path


def solve(d):
    graph = {}

    for line in d:
        a, b = line.split(')')
        graph[b] = a
        if a not in graph:
            graph[a] = 'STOP'

    you_orbits = pathize(graph, graph['YOU'])
    san_orbits = pathize(graph, graph['SAN'])

    best = 10**10

    for k, v in you_orbits.items():
        if k not in san_orbits:
            continue

        best = min(best, v + san_orbits[k])

    return best

def read_and_solve():
    with open('input_6.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
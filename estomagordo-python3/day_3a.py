import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(wires):
    aplaces = set()
    closest = 10**10

    apos = [0, 0]

    for instruction in wires[0].split(','):
        direction = instruction[0]
        dist = int(instruction[1:])

        delta = [1, 0]

        if direction == 'R':
            delta = [0, 1]
        if direction == 'L':
            delta = [0, -1]
        if direction == 'D':
            delta = [-1, 0]

        for x in range(dist):
            apos = [apos[0] + delta[0], apos[1] + delta[1]]
            aplaces.add(tuple(apos))

    bpos = [0, 0]

    for instruction in wires[1].split(','):
        direction = instruction[0]
        dist = int(instruction[1:])

        delta = [1, 0]

        if direction == 'R':
            delta = [0, 1]
        if direction == 'L':
            delta = [0, -1]
        if direction == 'D':
            delta = [-1, 0]

        for x in range(dist):
            bpos = [bpos[0] + delta[0], bpos[1] + delta[1]]
            if tuple(bpos) in aplaces:
                closest = min(closest, abs(bpos[0]) + abs(bpos[1]))

    return closest

def read_and_solve():
    with open('input_3.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
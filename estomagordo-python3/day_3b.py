import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def wire_to_places(wire):
    places = {}
    steps = 0

    pos = [0, 0]

    for instruction in wire:
        direction = instruction[0]
        dist = int(instruction[1:])

        delta = [1, 0]

        if direction == 'R':
            delta = [0, 1]
        if direction == 'L':
            delta = [0, -1]
        if direction == 'D':
            delta = [-1, 0]

        for _ in range(dist):
            steps += 1
            pos = [pos[0] + delta[0], pos[1] + delta[1]]
            places[tuple(pos)] = steps

    return places


def solve(wires):
    aplaces = wire_to_places(wires[0].split(','))
    bplaces = wire_to_places(wires[1].split(','))
    
    return min(aplaces[k] + bplaces[k] for k in aplaces.keys() & bplaces.keys())

def read_and_solve():
    with open('input_3.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
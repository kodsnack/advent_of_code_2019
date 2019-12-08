import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def decode(layers, width, height):
    image = [['2' for w in range(width)] for h in range(height)]

    for y in range(height):
        for x in range(width):
            for layer in layers:
                if layer[y][x] != '2':
                    image[y][x] = ' ' if layer[y][x] == '0' else '*'
                    break

    return image


def solve(d, width, height):
    layers = []
    layer = []
    row = []

    for i, c in enumerate(d):
        row.append(c)
        
        if i % width == width - 1:
            layer.append(row)
            row = []

        if i % (width * height) == (width * height) - 1:
            layers.append(layer)
            layer = []

    return '\n'.join(''.join(row) for row in decode(layers, width, height))

def read_and_solve():
    with open('input_8.txt') as f:
        data = f.readline().rstrip()
        return solve(data, 25, 6)

if __name__ == '__main__':
    print(read_and_solve())
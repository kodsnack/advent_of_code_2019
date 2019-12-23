import re

from functools import reduce


def get_moves(height, width, y, x):
    moves = []

    if y > 0:
        moves.append([y - 1, x])
    if y < height - 1:
        moves.append([y + 1, x])
    if x > 0:
        moves.append([y, x - 1])
    if x < width - 1:
        moves.append([y, x + 1])

    return moves


def gcd(a, b):
    for x in range(min(a, b), 0, -1):
        if a % x == 0 and b % x == 0:
            return x


def lcm(a, b):
    return (a * b) // gcd(a, b)


def lcm_many(nums):
    return reduce(lambda a, b: lcm(a, b), nums)


def absdist(coords):
    return sum(abs(x) for x in coords)


def coord_diff(a, b):
    return [a[x] - b[x] for x in range(len(a))]


def manhattan(a, b):
    return absdist(coord_diff(a, b))


def ints_from_line(line):
    pattern = re.compile(r'-?\d+')

    return [int(val) for val in re.findall(pattern, line) if val]
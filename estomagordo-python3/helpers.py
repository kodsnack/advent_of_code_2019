from functools import reduce


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
#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-02

import unittest

def fuel_required(mass):
    return mass // 3 - 2


def fuel_required2(mass):
    total = 0
    while True:
        mass = fuel_required(mass)
        if mass <= 0:
            return total
        total += mass


def solve(fuel_func):
    with open("input/day1.txt") as f:
        lines = f.readlines()
    sum = 0
    for line in lines:
        sum += fuel_func(int(line))
    print("{}".format(sum))


def solve1():
    solve(fuel_required)


def solve2():
    solve(fuel_required2)


class TestThis(unittest.TestCase):
    def test_fuel_required(self):
        self.assertEqual(fuel_required(12), 2)
        self.assertEqual(fuel_required(14), 2)
        self.assertEqual(fuel_required(1969), 654)
        self.assertEqual(fuel_required(100756), 33583)

    def test_fuel_required2(self):
        self.assertEqual(fuel_required2(12), 2)
        self.assertEqual(fuel_required2(14), 2)
        self.assertEqual(fuel_required2(1969), 966)
        self.assertEqual(fuel_required2(100756), 50346)


if __name__ == "__main__":
    #unittest.main()
    solve1()
    solve2()

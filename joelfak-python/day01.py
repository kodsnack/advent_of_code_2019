#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
from functools import reduce

def calcFuel(mass):
    return mass // 3 - 2

def part1(data):
    return reduce((lambda x, y: x+y), map(calcFuel, data))

def calcFuelExtended(mass):
    fuel = calcFuel(mass)
    if fuel <= 0:
        return 0
    else:
        return fuel + calcFuelExtended(fuel)

def part2(data):
    return reduce((lambda x, y: x+y), map(calcFuelExtended, data))

## Unit tests ########################################################

class TestDay01(unittest.TestCase):
    def test_calcFuel_12(self):
        self.assertEqual(calcFuel(12), 2)

    def test_calcFuel_14(self):
        self.assertEqual(calcFuel(14), 2)

    def test_calcFuel_1969(self):
        self.assertEqual(calcFuel(1969), 654)

    def test_calcFuel_100756(self):
        self.assertEqual(calcFuel(100756), 33583)

    def test_part1(self):
        self.assertEqual(part1([12, 14, 1969, 100756]), 34241)

    def test_calcFuelExtended_14(self):
        self.assertEqual(calcFuelExtended(14), 2)

    def test_calcFuelExtended_1969(self):
        self.assertEqual(calcFuelExtended(1969), 966)

    def test_calcFuelExtended_100756(self):
        self.assertEqual(calcFuelExtended(100756), 50346)

    def test_part2(self):
        self.assertEqual(part2([14, 1969, 100756]), 51314)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 1")
    print("Part1 result: {}".format(part1(getIntsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getIntsFromFile(sys.argv[1]))))

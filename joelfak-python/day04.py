#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys

def isPasswordValid(password, countPairsInLargerGroups):
    passwordValid = True

    if password < 100000 or password > 999999:
        passwordValid = False

    passwordstr = str(password)

    if countPairsInLargerGroups:
        if sum(1 for x in zip(passwordstr, passwordstr[1:]) if x[0] == x[1]) < 1:
            passwordValid = False
    else:
        lastNumber = None
        numConsecutiveNumbers = 0
        numPairs = 0
        for c in passwordstr:
            if int(c) == lastNumber:
                numConsecutiveNumbers = numConsecutiveNumbers + 1
            else:
                numConsecutiveNumbers = 1
            lastNumber = int(c)

            if numConsecutiveNumbers == 2:
                numPairs = numPairs + 1
            if numConsecutiveNumbers == 3:
                numPairs = numPairs - 1
        if numPairs < 1:
            passwordValid = False

    if sum(1 for x in zip(passwordstr, passwordstr[1:]) if x[0] > x[1]) > 0:
        passwordValid = False

    return passwordValid

def part1(minVal, maxVal):
    return sum(1 for x in range(minVal, maxVal+1) if isPasswordValid(x, True))

def part2(minVal, maxVal):
    return sum(1 for x in range(minVal, maxVal+1) if isPasswordValid(x, False))

## Unit tests ########################################################

class TestDay04(unittest.TestCase):
    def test_isPasswordValid_part_1_example_1(self):
        self.assertEqual(isPasswordValid(111111, True), True)

    def test_isPasswordValid_part_1_example_2(self):
        self.assertEqual(isPasswordValid(223450, True), False)

    def test_isPasswordValid_part_1_example_3(self):
        self.assertEqual(isPasswordValid(123789, True), False)

    def test_isPasswordValid_part_2_example_1(self):
        self.assertEqual(isPasswordValid(112233, False), True)

    def test_isPasswordValid_part_2_example_2(self):
        self.assertEqual(isPasswordValid(123444, False), False)

    def test_isPasswordValid_part_2_example_3(self):
        self.assertEqual(isPasswordValid(111122, False), True)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 4")
    print("Part1 result: {}".format(part1(272091,815432)))
    print("Part2 result: {}".format(part2(272091,815432)))

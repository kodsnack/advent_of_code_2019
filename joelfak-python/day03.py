#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
import re
from collections import namedtuple
import matplotlib.pyplot as plt

Point = namedtuple('Point', ['x', 'y'])

def getCoordinatesForWire(wire, color=None):
    coords = []
    pos = Point(0,0)

    for path in wire:
        match = re.match(r"([A-Z])([0-9]+)", path)
        if match:
            items = match.groups()
            direction = match.group(1)
            distance = int(match.group(2))

            startPos = pos

            for i in range(distance):
                if direction == 'R':
                    pos = Point(pos.x + 1, pos.y)
                if direction == 'L':
                    pos = Point(pos.x - 1, pos.y)
                if direction == 'U':
                    pos = Point(pos.x, pos.y + 1)
                if direction == 'D':
                    pos = Point(pos.x, pos.y - 1)

                coords.append(pos)
    return coords

def plot(coordsA, coordsB):
    for path in zip(coordsA, coordsA[1:]):
        plt.plot([path[0].x, path[1].x], [path[0].y, path[1].y], 'ro-')
    for path in zip(coordsB, coordsB[1:]):
        plt.plot([path[0].x, path[1].x], [path[0].y, path[1].y], 'bo-')
    plt.axis('equal')
    plt.show()

def findNearestIntersection(wireA, wireB, enablePlot=False):
    coordsA = getCoordinatesForWire(wireA)
    coordsB = getCoordinatesForWire(wireB)

    intersections = set(coordsA).intersection(coordsB)

    if enablePlot:
        plot(coordsA, coordsB)

    return min(map(lambda x: abs(x[0])+abs(x[1]), intersections))


def part1(directions):
    return findNearestIntersection(directions[0], directions[1])


def findNearestIntersection2(wireA, wireB, enablePlot=False):
    coordsA = getCoordinatesForWire(wireA)
    coordsB = getCoordinatesForWire(wireB)

    intersections = set(coordsA).intersection(coordsB)

    if enablePlot:
        plot(coordsA, coordsB)

    return min(map(lambda x: coordsA.index(x) + coordsB.index(x) + 2, intersections))


def part2(directions):
    return findNearestIntersection2(directions[0], directions[1])

## Unit tests ########################################################

class TestDay03(unittest.TestCase):
    def test_findNearestIntersection_example_1(self):
        self.assertEqual(findNearestIntersection(["R8","U5","L5","D3"], ["U7","R6","D4","L4"]), 6)

    def test_findNearestIntersection_example_1_inverse(self):
        self.assertEqual(findNearestIntersection(["L8","D5","R5","U3"], ["D7","L6","U4","R4"]), 6)

    def test_findNearestIntersection_example_2(self):
        self.assertEqual(findNearestIntersection(["R75","D30","R83","U83","L12","D49","R71","U7","L72"], ["U62","R66","U55","R34","D71","R55","D58","R83"]), 159)

    def test_findNearestIntersection_example_3(self):
        self.assertEqual(findNearestIntersection(["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"], ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]), 135)

    ## Part 2 #########################################################

    def test_findNearestIntersection_part2_example_1(self):
        self.assertEqual(findNearestIntersection2(["R8","U5","L5","D3"], ["U7","R6","D4","L4"]), 30)

    def test_findNearestIntersection_part2_example_2(self):
        self.assertEqual(findNearestIntersection2(["R75","D30","R83","U83","L12","D49","R71","U7","L72"], ["U62","R66","U55","R34","D71","R55","D58","R83"]), 610)

    def test_findNearestIntersection_part2_example_3(self):
        self.assertEqual(findNearestIntersection2(["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"], ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]), 410)


## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 3")
    print("Part1 result: {}".format(part1(getCommaSeparatedStringsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getCommaSeparatedStringsFromFile(sys.argv[1]))))
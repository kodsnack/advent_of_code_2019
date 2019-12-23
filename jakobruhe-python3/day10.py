#!/usr/bin/env python3

# By Jakob Ruhe (jakob.ruhe@gmail.com) 2019-12-10

import unittest
from collections import namedtuple
import math

Point = namedtuple("Point", ('x', 'y'))


def parse_input(input: str):
    lines = input.strip().split()
    asteroids = {}
    for y,line in enumerate(lines):
        for x,c in enumerate(line):
            if c == '#':
                asteroids[Point(x,y)] = 0
    return asteroids


def dist(p1, p2):
    return math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))


def equals(a, b):
    return abs(a-b) <= 1e-6


def is_point_between(p, p1, p2):
    if not equals(dist(p, p1) + dist(p, p2), dist(p1, p2)):
        return False
    if p.x < min(p1.x, p2.x) or p.x > max(p1.x, p2.x):
        return False
    if p.y < min(p1.y, p2.y) or p.y > max(p1.y, p2.y):
        return False
    return True


def can_see(asteroid_points, p1, p2):
    for p in asteroid_points:
        if p == p1 or p == p2:
            continue
        if is_point_between(p, p1, p2):
            return False
    return True


def solve1(puzzle_input):
    asteroids = parse_input(puzzle_input)
    asteroids_left = list(asteroids.keys())
    while asteroids_left:
        p1 = asteroids_left.pop()
        for p2 in asteroids_left:
            if can_see(asteroids.keys(), p1, p2):
                asteroids[p1] += 1
                asteroids[p2] += 1
    best = max(asteroids.keys(), key=lambda key: asteroids[key])
    return asteroids[best], best


def angle(p1, p2):
    a = math.atan2(p2.x-p1.x, p1.y-p2.y)
    return a if a >= 0 else 2 * math.pi + a


def solve2(puzzle_input, giant_laser_pos, num_to_destroy):
    asteroids = parse_input(puzzle_input)
    asteroids_left = list(asteroids.keys())
    asteroids_left.remove(giant_laser_pos)
    num_destroyed = 0
    revolution = 0
    while True:
        while True:
            candidates = list(filter(lambda p: can_see(asteroids_left, giant_laser_pos, p), asteroids_left))
            candidates.sort(key=lambda p: angle(giant_laser_pos, p))
            for p in candidates:
                asteroids_left.remove(p)
                num_destroyed += 1
                if num_destroyed == num_to_destroy:
                    return p
            revolution += 1
    return None


# Execute tests with:
# python3 -m unittest dayX.py
class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(solve1(
            """
            .#..#
            .....
            #####
            ....#
            ...##
            """), (8, (3, 4)))
        self.assertEqual(solve1(
            """
            ......#.#.
            #..#.#....
            ..#######.
            .#.#.###..
            .#..#.....
            ..#....#.#
            #..#....#.
            .##.#..###
            ##...#..#.
            .#....####
            """), (33, Point(5, 8)))
        self.assertEqual(solve1(
            """
            #.#...#.#.
            .###....#.
            .#....#...
            ##.#.#.#.#
            ....#.#.#.
            .##..###.#
            ..#...##..
            ..##....##
            ......#...
            .####.###.
            """), (35, Point(1, 2)))
        self.assertEqual(solve1(
            """
            .#..#..###
            ####.###.#
            ....###.#.
            ..###.##.#
            ##.##.#.#.
            ....###..#
            ..#.#..#.#
            #..#.#.###
            .##...##.#
            .....#.#..
            """), (41, Point(6, 3)))
        self.assertEqual(solve1(
            """
            .#..##.###...#######
            ##.############..##.
            .#.######.########.#
            .###.#######.####.#.
            #####.##.#.##.###.##
            ..#####..#.#########
            ####################
            #.####....###.#.#.##
            ##.#################
            #####.##.###..####..
            ..######..##.#######
            ####.##.####...##..#
            .#####..#.######.###
            ##...#.##########...
            #.##########.#######
            .####.#.###.###.#.##
            ....##.##.###..#####
            .#.#.###########.###
            #.#.#.#####.####.###
            ###.##.####.##.#..##
            """), (210, Point(11, 13)))


    def test2(self):
        self.assertEqual(
            solve2(
                """
                .#....#####...#..
                ##...##.#####..##
                ##...#...#.#####.
                ..#.....#...###..
                ..#.#.....#....##
                """,
                Point(8, 3),
                36),
            Point(14, 3))
        self.assertEqual(
            solve2(
                """
                .#..##.###...#######
                ##.############..##.
                .#.######.########.#
                .###.#######.####.#.
                #####.##.#.##.###.##
                ..#####..#.#########
                ####################
                #.####....###.#.#.##
                ##.#################
                #####.##.###..####..
                ..######..##.#######
                ####.##.####...##..#
                .#####..#.######.###
                ##...#.##########...
                #.##########.#######
                .####.#.###.###.#.##
                ....##.##.###..#####
                .#.#.###########.###
                #.#.#.#####.####.###
                ###.##.####.##.#..##
                """,
                Point(11, 13),
                200),
            Point(8, 2))


if __name__ == "__main__":
    with open("input/day10.txt", "r") as f:
        input = f.read()
    num, p1 = solve1(input)
    print("P1: {} at {}".format(num, p1))
    #giant_laser_pos = p1
    giant_laser_pos = Point(22, 28)
    p2 = solve2(input, giant_laser_pos, 200)
    print("P2: {} at: {}".format(p2.x * 100 + p2.y, p2))

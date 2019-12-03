#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-03

import unittest
from collections import namedtuple
from collections import defaultdict

Loc = namedtuple("Loc", ("x", "y"))
DirDist = namedtuple("DirDist", ("direction", "distance"))

DIRECTIONS = {
    'U': Loc(0, 1),
    'D': Loc(0, -1),
    'L': Loc(-1, 0),
    'R': Loc(1, 0),
}


def parse_wire_path(input: str):
    return list(map(lambda s: DirDist(s[0], int(s[1:])), input.split(",")))


def dir_x(direction):
    return DIRECTIONS[direction].x


def dir_y(direction):
    return DIRECTIONS[direction].y


def put_location_on_board(board, loc, name, dist):
    if name in board[loc]:
        return
    board[loc][name] = dist


def put_segment_on_board(board, start_loc, start_dist, segment, name):
    loc = start_loc
    dist = start_dist
    for d in range(0, segment.distance):
        loc = Loc(loc.x + dir_x(segment.direction), loc.y + dir_y(segment.direction))
        dist += 1
        put_location_on_board(board, loc, name, dist)
    return loc, dist


def put_wire_path_on_board(board, wire_path, name):
    loc = Loc(0, 0)
    dist = 0
    for segment in wire_path:
        loc, dist = put_segment_on_board(board, loc, dist, segment, name)


def manhattan_distance(p1, p2):
    return abs(p1.x - p2.x) + abs(p1.y - p2.y)


def closest_crossing(board):
    closest = None
    for loc, wire_paths in board.items():
        if len(wire_paths) >= 2 and loc != Loc(0, 0):
            dist = manhattan_distance(Loc(0, 0), loc)
            closest = dist if closest is None or dist < closest else closest
    return closest


def draw(board):
    for y in range(10, -10, -1):
        for x in range(-10, 12):
            if x == 0 and y == 0:
                print("o", end="")
            elif Loc(x, y) in board:
                if len(board[Loc(x,y)]) >= 2:
                    print("X", end="")
                else:
                    print("+", end="")
            else:
                print(".", end="")
        print("")


def solve1(input):
    lines = input.split("\n")
    board = defaultdict(dict)
    for name, line in enumerate(lines):
        put_wire_path_on_board(board, parse_wire_path(line), name)
    #draw(board)
    return closest_crossing(board)


def closest_crossing_distance(board):
    closest = None
    for loc, wire_paths in board.items():
        if len(wire_paths) >= 2 and loc != Loc(0, 0):
            dist = sum(wire_paths.values())
            closest = dist if closest is None or dist < closest else closest
    return closest


def solve2(input):
    lines = input.split("\n")
    board = defaultdict(dict)
    for name, line in enumerate(lines):
        put_wire_path_on_board(board, parse_wire_path(line), name)
    return closest_crossing_distance(board)


class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(solve1("R8,U5,L5,D3\nU7,R6,D4,L4"), 6)
        self.assertEqual(solve1("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"), 159)
        self.assertEqual(solve1("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"), 135)
    def test2(self):
        self.assertEqual(solve2("R8,U5,L5,D3\nU7,R6,D4,L4"), 30)
        self.assertEqual(solve2("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"), 610)
        self.assertEqual(solve2("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"), 410)


if __name__ == "__main__":
    #unittest.main()
    with open("input/day3.txt", "r") as f:
        input = f.read()
    a1 = solve1(input.strip())
    print("{}".format(a1))
    a2 = solve2(input.strip())
    print("{}".format(a2))

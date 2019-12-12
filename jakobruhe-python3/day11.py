#!/usr/bin/env python3

# By Jakob Ruhe (jakob.ruhe@gmail.com) 2019-12-11 at 20:53

import unittest
from collections import defaultdict
from collections import namedtuple

DIR_UP = 0
DIR_RIGHT = 1
DIR_DOWN = 2
DIR_LEFT = 3
DIRECTIONS = (DIR_UP, DIR_RIGHT, DIR_DOWN, DIR_LEFT)

COLOR_BLACK = 0
COLOR_WHITE = 1

TURN_LEFT = 0
TURN_RIGHT = 1

Point = namedtuple("Point", ('x', 'y'))


def parse_input(input: str):
    return list(map(int, input.strip().split(",")))


def dir_x(direction):
    if direction == DIR_LEFT:
        return -1
    elif direction == DIR_RIGHT:
        return 1
    else:
        return 0


def dir_y(direction):
    if direction == DIR_UP:
        return -1
    elif direction == DIR_DOWN:
        return 1
    else:
        return 0


class Computer:
    def __init__(self, program):
        self.memory = defaultdict(int)
        for i, b in enumerate(program):
            self.memory[i] = b
        self.input = []
        self.pc = 0
        self.rel_base_offset = 0

    def get_address(self, mode, offset):
        if mode == 0:
            return self.memory[offset]
        elif mode == 1:
            return offset
        elif mode == 2:
            return self.memory[offset] + self.rel_base_offset
        else:
            raise ValueError("Bad param mode: {} pc: {}".format(mode, self.pc))

    def fetch_parameters(self, num):
        modes = self.memory[self.pc] // 100
        params = []
        for i in range(0, num):
            m = modes % 10
            params.append(self.get_address(m, self.pc + i + 1))
            modes = modes // 10
        return params

    def run_until_complete(self, input):
        result = []
        output = self.run_until_output(input)
        while output is not None:
            result.append(output)
            output = self.run_until_output()
        return result

    def run_until_output(self, input):
        self.input.extend(input)
        while True:
            op_code = self.memory[self.pc] % 100
            if op_code == 1:
                params = self.fetch_parameters(3)
                self.memory[params[-1]] = self.memory[params[0]] + self.memory[params[1]]
                self.pc += len(params) + 1
            elif op_code == 2:
                params = self.fetch_parameters(3)
                self.memory[params[-1]] = self.memory[params[0]] * self.memory[params[1]]
                self.pc += len(params) + 1
            elif op_code == 3:
                params = self.fetch_parameters(1)
                self.memory[params[-1]] = self.input.pop(0)
                self.pc += len(params) + 1
            elif op_code == 4:
                params = self.fetch_parameters(1)
                self.pc += len(params) + 1
                return self.memory[params[0]]
            elif op_code == 5:
                params = self.fetch_parameters(2)
                if self.memory[params[0]]:
                    self.pc = self.memory[params[1]]
                else:
                    self.pc += len(params) + 1
            elif op_code == 6:
                params = self.fetch_parameters(2)
                if not self.memory[params[0]]:
                    self.pc = self.memory[params[1]]
                else:
                    self.pc += len(params) + 1
            elif op_code == 7:
                params = self.fetch_parameters(3)
                self.memory[params[-1]] = 1 if self.memory[params[0]] < self.memory[params[1]] else 0
                self.pc += len(params) + 1
            elif op_code == 8:
                params = self.fetch_parameters(3)
                self.memory[params[-1]] = 1 if self.memory[params[0]] == self.memory[params[1]] else 0
                self.pc += len(params) + 1
            elif op_code == 9:
                params = self.fetch_parameters(1)
                self.rel_base_offset += self.memory[params[0]]
                self.pc += len(params) + 1
            elif op_code == 99:
                return None
            else:
                raise ValueError("Unknown op_code: {} at {}".format(op_code, self.pc))


def solve(prog, color_on_start_panel):
    computer = Computer(prog)
    location = Point(0, 0)
    direction = DIR_UP
    board = {location: color_on_start_panel}
    while True:
        current_color = board[location] if location in board else COLOR_BLACK
        new_color = computer.run_until_output([current_color])
        if new_color is None:
            return board
        board[location] = new_color
        turn = computer.run_until_output([])
        if turn == TURN_LEFT:
            direction = (direction - 1) % len(DIRECTIONS)
        else:
            direction = (direction + 1) % len(DIRECTIONS)
        location = Point(location.x + dir_x(direction), location.y + dir_y(direction))


def solve1(program):
    board = solve(program, COLOR_BLACK)
    print("P1: {}".format(len(board)))


def print_board(board):
    all_x = [p.x for p in board.keys()]
    all_y = [p.y for p in board.keys()]
    min_x = min(all_x)
    max_x = max(all_x)
    min_y = min(all_y)
    max_y = max(all_y)
    for y in range(min_y, max_y + 1):
        line = []
        for x in range(min_x, max_x + 1):
            color = board.get(Point(x,y), None)
            if color == COLOR_BLACK:
                line.append(' ')
            elif color == COLOR_WHITE:
                line.append('w')
            else:
                line.append(' ')
        print("".join(line))


def solve2(program):
    board = solve(program, COLOR_WHITE)
    print("P2:")
    print_board(board)


if __name__ == "__main__":
    with open("input/day11.txt", "r") as f:
        input = f.read()
    program = parse_input(input)
    solve1(program)
    solve2(program)

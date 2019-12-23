#!/usr/bin/env python3

# By Jakob Ruhe (jakob.ruhe@gmail.com) 2019-12-09 at 06:24

import unittest
from collections import defaultdict


def parse_input(input: str):
    return list(map(int, input.strip().split(",")))

class Amp:
    def __init__(self, program, input = []):
        self.memory = defaultdict(int)
        for i, b in enumerate(program):
            self.memory[i] = b
        self.input = input
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

    def run_until_complete(self, input=[]):
        result = []
        output = self.run_until_output(input)
        while output is not None:
            result.append(output)
            output = self.run_until_output()
        return result

    def run_until_output(self, input=[]):
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


def solve1(program):
    amp = Amp(program, [1])
    return amp.run_until_complete()


def solve2(program):
    amp = Amp(program, [2])
    return amp.run_until_complete()


# Execute tests with:
# python3 -m unittest dayX.py
class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(solve1(parse_input("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")),
                         [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99])
        self.assertEqual(solve1(
            parse_input("1102,34915192,34915192,7,4,7,99,0")), [1219070632396864])
        self.assertEqual(solve1(
            parse_input("104,1125899906842624,99")), [1125899906842624])

    def test2(self):
        pass


if __name__ == "__main__":
    with open("input/day9.txt", "r") as f:
        input = f.read()
    program = parse_input(input)
    print("P1: {}".format(solve1(program)))
    print("P2: {}".format(solve2(program)))

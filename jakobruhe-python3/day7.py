#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-07
# Started at 06:10
# P1 solved at 06:34
# P2 solved at 07:25

import unittest
import itertools


def parse_input(input: str):
    return list(map(int, input.split(",")))


def fetch_parameters(memory, pc, modes, num):
    params = []
    for i in range(0, num):
        m = modes % 10
        modes = modes // 10
        if m == 0:
            params.append(memory[memory[pc + 1 + i]])
        elif m == 1:
            params.append(memory[pc + 1 + i])
        else:
            raise ValueError("Bad param mode: {} pc: {}".format(m, pc))
    return params


class Amp:
    def __init__(self, program, input):
        self.memory = program.copy()
        self.pc = 0
        self.input = input

    def run_until_output(self, input=[]):
        self.input.extend(input)
        while True:
            op_code = self.memory[self.pc] % 100
            if op_code == 1:
                params = fetch_parameters(self.memory, self.pc, self.memory[self.pc] // 100, 2)
                self.memory[self.memory[self.pc + 3]] = params[0] + params[1]
                self.pc += 4
            elif op_code == 2:
                params = fetch_parameters(self.memory, self.pc, self.memory[self.pc] // 100, 2)
                self.memory[self.memory[self.pc + 3]] = params[0] * params[1]
                self.pc += 4
            elif op_code == 3:
                self.memory[self.memory[self.pc + 1]] = self.input.pop(0)
                self.pc += 2
            elif op_code == 4:
                params = fetch_parameters(self.memory, self.pc, self.memory[self.pc] // 100, 1)
                self.pc += 2
                return params[0]
            elif op_code == 5:
                params = fetch_parameters(self.memory, self.pc, self.memory[self.pc] // 100, 2)
                if params[0]:
                    self.pc = params[1]
                else:
                    self.pc += 3
            elif op_code == 6:
                params = fetch_parameters(self.memory, self.pc, self.memory[self.pc] // 100, 2)
                if not params[0]:
                    self.pc = params[1]
                else:
                    self.pc += 3
            elif op_code == 7:
                params = fetch_parameters(self.memory, self.pc, self.memory[self.pc] // 100, 3)
                self.memory[self.memory[self.pc + 3]] = 1 if params[0] < params[1] else 0
                self.pc += 4
            elif op_code == 8:
                params = fetch_parameters(self.memory, self.pc, self.memory[self.pc] // 100, 3)
                self.memory[self.memory[self.pc + 3]] = 1 if params[0] == params[1] else 0
                self.pc += 4
            elif op_code == 99:
                return None
            else:
                raise ValueError("Unknown op_code: {} at {}".format(op_code, self.pc))


def calculate_output1(program, phase_settings):
    output = 0
    for p in phase_settings:
        amp = Amp(program, [p, output])
        output = amp.run_until_output()
    return output


def solve1(program):
    highest_output = None
    base_phase_settings = list(range(5))
    for phase_settings in itertools.permutations(base_phase_settings):
        output = calculate_output1(program, phase_settings)
        if highest_output is None or output > highest_output:
            highest_output = output
    return highest_output


def calculate_output2(program, phase_settings):
    output = 0
    amps = [Amp(program, [p]) for p in phase_settings]
    while True:
        for amp in amps:
            output_or_none = amp.run_until_output([output])
            if output_or_none is None:
                return output
            output = output_or_none


def solve2(program):
    highest_output = None
    base_phase_settings = list(range(5, 10))
    for phase_settings in itertools.permutations(base_phase_settings):
        output = calculate_output2(program, phase_settings)
        if highest_output is None or output > highest_output:
            highest_output = output
    return highest_output


# Execute tests with:
# python3 -m unittest dayX.py
class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(solve1(parse_input("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")), 43210)
        self.assertEqual(solve1(
            parse_input("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")), 54321)
        self.assertEqual(solve1(
            parse_input(
                "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")),
            65210)

    def test2(self):
        self.assertEqual(solve2(parse_input(
            "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")),
            139629729)
        self.assertEqual(solve2(parse_input(
            "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")),
            18216)


if __name__ == "__main__":
    with open("input/day7.txt", "r") as f:
        input = f.read()
    program = parse_input(input)
    print("P1: {}".format(solve1(program)))
    print("P2: {}".format(solve2(program)))

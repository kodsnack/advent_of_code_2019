#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-05 at around 06:20

import unittest

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


def execute(program, input):
    output = 0
    memory = program.copy()
    pc = 0
    while True:
        op_code = memory[pc] % 100
        if op_code == 1:
            params = fetch_parameters(memory, pc, memory[pc] // 100, 2)
            memory[memory[pc+3]] = params[0] + params[1]
            pc += 4
        elif op_code == 2:
            params = fetch_parameters(memory, pc, memory[pc] // 100, 2)
            memory[memory[pc+3]] = params[0] * params[1]
            pc += 4
        elif op_code == 3:
            memory[memory[pc + 1]] = input
            pc += 2
        elif op_code == 4:
            params = fetch_parameters(memory, pc, memory[pc] // 100, 1)
            output = params[0]
            pc += 2
        elif op_code == 5:
            params = fetch_parameters(memory, pc, memory[pc] // 100, 2)
            if params[0]:
                pc = params[1]
            else:
                pc += 3
        elif op_code == 6:
            params = fetch_parameters(memory, pc, memory[pc] // 100, 2)
            if not params[0]:
                pc = params[1]
            else:
                pc += 3
        elif op_code == 7:
            params = fetch_parameters(memory, pc, memory[pc] // 100, 3)
            memory[memory[pc + 3]] = 1 if params[0] < params[1] else 0
            pc += 4
        elif op_code == 8:
            params = fetch_parameters(memory, pc, memory[pc] // 100, 3)
            memory[memory[pc + 3]] = 1 if params[0] == params[1] else 0
            pc += 4
        elif op_code == 99:
            return output
        else:
            raise ValueError("Unknown op_code: {} at {}".format(op_code, pc))


def solve1():
    with open("input/day5.txt", "r") as f:
        input = f.read()
    program = parse_input(input)
    result = execute(program, 1)
    print("{}".format(result))


def solve2():
    with open("input/day5.txt", "r") as f:
        input = f.read()
    program = parse_input(input)
    result = execute(program, 5)
    print("{}".format(result))


class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(execute(parse_input("1002,4,3,4,33")), [1002,4,3,4,99])


if __name__ == "__main__":
    solve1()
    solve2()


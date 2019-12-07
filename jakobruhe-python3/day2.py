#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-02

import unittest

def parse_input(input: str):
    return list(map(int, input.split(",")))


def execute(program):
    memory = program.copy()
    pc = 0
    while True:
        op_code = memory[pc]
        if op_code == 1:
            memory[memory[pc+3]] = memory[memory[pc+1]] + memory[memory[pc+2]]
        elif op_code == 2:
            memory[memory[pc+3]] = memory[memory[pc+1]] * memory[memory[pc+2]]
        elif op_code == 99:
            return memory
        else:
            raise ValueError("Unknown op_code: {} at {}".format(op_code, pc))
        pc += 4


def solve1():
    with open("input/day2.txt", "r") as f:
        input = f.read()
    program = parse_input(input)
    program[1] = 12
    program[2] = 2
    memory = execute(program)
    print("{}".format(memory[0]))


def solve2():
    with open("input/day2.txt", "r") as f:
        input = f.read()
    program = parse_input(input)
    for noun in range(0, 100):
        for verb in range(0, 100):
            program[1] = noun
            program[2] = verb
            memory = execute(program)
            if memory[0] == 19690720:
                print("Noun: {}, verb: {}, result: {}".format(noun, verb, noun * 100 + verb))
                return


class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(execute(parse_input("1,0,0,0,99")), [2,0,0,0,99])
        self.assertEqual(execute(parse_input("2,3,0,3,99")), [2,3,0,6,99])
        self.assertEqual(execute(parse_input("2,4,4,5,99,0")), [2,4,4,5,99,9801])
        self.assertEqual(execute(parse_input("1,1,1,4,99,5,6,0,99")), [30,1,1,4,2,5,6,0,99])


if __name__ == "__main__":
    #unittest.main()
    solve1()
    solve2()

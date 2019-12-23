#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
import operator

ops = {
    1: operator.add,
    2: operator.mul
}

def intcode(program):
    p = 0
    opcode = program[p]

    while opcode != 99:
        program[program[p+3]] = ops[opcode](program[program[p+1]], program[program[p+2]])
        p = p + 4
        opcode = program[p]

    return program

def part1(program):
    program[1] = 12
    program[2] = 2
    return intcode(program)[0]

def part2(program):
    resFound = False
    for noun in range(100):
        for verb in range(100):
            program_copy = program.copy()
            program_copy[1] = noun
            program_copy[2] = verb
            res = intcode(program_copy)[0]
            if res == 19690720:
                resFound = True
                print("Found it!")
                print("noun: {}, verb: {}".format(noun, verb))
                break
        else:
            continue
        break

    print("noun: {}, verb: {}".format(noun, verb))
    return 100 * noun + verb

## Unit tests ########################################################

class TestDay02(unittest.TestCase):
    def test_intcode_example_program_1(self):
        self.assertEqual(intcode([1,0,0,0,99]), [2,0,0,0,99])

    def test_intcode_example_program_2(self):
        self.assertEqual(intcode([2,3,0,3,99]), [2,3,0,6,99])

    def test_intcode_example_program_3(self):
        self.assertEqual(intcode([2,4,4,5,99,0]), [2,4,4,5,99,9801])

    def test_intcode_example_program_4(self):
        self.assertEqual(intcode([1,1,1,4,99,5,6,0,99]), [30,1,1,4,2,5,6,0,99])

    def test_intcode_example_program(self):
        self.assertEqual(intcode([1,9,10,3,2,3,11,0,99,30,40,50]), [3500,9,10,70,2,3,11,0,99,30,40,50])

    def test_part1(self):
        self.assertEqual(part1([1,9,10,0,99,3,11,0,99,30,40,50,60]), 62)


## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 2")
    print("Part1 result: {}".format(part1(getCommaSeparatedIntsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getCommaSeparatedIntsFromFile(sys.argv[1]))))

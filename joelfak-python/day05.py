#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
import operator
from collections import namedtuple, deque

IntcodeResult = namedtuple('IntcodeResult', ['program', 'output'])
OpCode = namedtuple('OpCode', ['opcode','parameterModes'])

def parseOpcode(opcodeIn):
    parameterModes = deque()
    opcode = opcodeIn % 100
    opcodeIn //= 100
    while opcodeIn:
        parameterModes.append(opcodeIn % 10)
        opcodeIn //= 10

    return OpCode(opcode, parameterModes)

def getParameter(program, p, parameterModes):
    if len(parameterModes) > 0 and parameterModes.popleft():
        return program[p]
    else:
        return program[program[p]]

def intcode(program, input=None):
    p = 0
    oc = parseOpcode(program[p])

    output = []

    while oc.opcode != 99:
        if oc.opcode == 1: # Addition
            program[program[p+3]] = getParameter(program, p+1, oc.parameterModes) + getParameter(program, p+2, oc.parameterModes)
            p = p + 4
        elif oc.opcode == 2: # Multiplication
            program[program[p+3]] = getParameter(program, p+1, oc.parameterModes) * getParameter(program, p+2, oc.parameterModes)
            p = p + 4
        elif oc.opcode == 3: # Read input
            program[program[p+1]] = input
            p = p + 2
        elif oc.opcode == 4: # Write output
            output.append(getParameter(program, p+1, oc.parameterModes))
            p = p + 2
        elif oc.opcode == 5: # Jump if true
            if getParameter(program, p+1, oc.parameterModes) != 0:
                p = getParameter(program, p+2, oc.parameterModes)
            else:
                p = p + 3
        elif oc.opcode == 6: # Jump if false
            if getParameter(program, p+1, oc.parameterModes) == 0:
                p = getParameter(program, p+2, oc.parameterModes)
            else:
                p = p + 3
        elif oc.opcode == 7: # Less than
            if getParameter(program, p+1, oc.parameterModes) < getParameter(program, p+2, oc.parameterModes):
                program[program[p+3]] = 1
            else:
                program[program[p+3]] = 0
            p = p + 4
        elif oc.opcode == 8: # Equals
            if getParameter(program, p+1, oc.parameterModes) == getParameter(program, p+2, oc.parameterModes):
                program[program[p+3]] = 1
            else:
                program[program[p+3]] = 0
            p = p + 4
        else:
            raise Exception('Invalid operator', oc.opcode)
        oc = parseOpcode(program[p])

    return IntcodeResult(program, output)

def part1(program):
    return intcode(program, 1).output

def part2(program):
    return intcode(program, 5).output

## Unit tests ########################################################

class TestDay02(unittest.TestCase):
    def test_intcode_example_program_1(self):
        self.assertEqual(intcode([1,0,0,0,99]).program, [2,0,0,0,99])

    def test_intcode_example_program_2(self):
        self.assertEqual(intcode([2,3,0,3,99]).program, [2,3,0,6,99])

    def test_intcode_example_program_3(self):
        self.assertEqual(intcode([2,4,4,5,99,0]).program, [2,4,4,5,99,9801])

    def test_intcode_example_program_4(self):
        self.assertEqual(intcode([1,1,1,4,99,5,6,0,99]).program, [30,1,1,4,2,5,6,0,99])

    def test_intcode_example_program(self):
        self.assertEqual(intcode([1,9,10,3,2,3,11,0,99,30,40,50]).program, [3500,9,10,70,2,3,11,0,99,30,40,50])

class TestParseOpcode(unittest.TestCase):
    def test_1(self):
        self.assertEqual(parseOpcode(1002), (2, deque([0, 1])))

    def test_2(self):
        self.assertEqual(parseOpcode(101003), (3, deque([0, 1, 0, 1])))

class TestDay05_part1(unittest.TestCase):
    def test_intcode_output_is_input_1(self):
        self.assertEqual(intcode([3,0,4,0,99], 3).output[0], 3)

    def test_intcode_output_is_input_2(self):
        self.assertEqual(intcode([3,0,4,0,99], 7).output[0], 7)

    def test_intcode_parameter_mode(self):
        self.assertEqual(intcode([1002,4,3,4,33]).program, [1002,4,3,4,99])

    def test_intcode_negative_numbers(self):
        self.assertEqual(intcode([1101,100,-1,4,0]).program, [1101,100,-1,4,99])

class TestDay05_part2(unittest.TestCase):
    def test_intcode_if_input_is_8_return_1(self):
        self.assertEqual(intcode([3,9,8,9,10,9,4,9,99,-1,8], 8).output[0], 1)
    def test_intcode_if_input_is_not_8_return_0(self):
        self.assertEqual(intcode([3,9,8,9,10,9,4,9,99,-1,8], 3).output[0], 0)

    def test_intcode_if_input_is_less_than_8_return_1(self):
        self.assertEqual(intcode([3,9,7,9,10,9,4,9,99,-1,8], 7).output[0], 1)
    def test_intcode_if_input_is_not_less_than_8_return_0(self):
        self.assertEqual(intcode([3,9,7,9,10,9,4,9,99,-1,8], 8).output[0], 0)

    def test_intcode_immediate_mode_if_input_is_8_return_1(self):
        self.assertEqual(intcode([3,3,1108,-1,8,3,4,3,99], 8).output[0], 1)
    def test_intcode_immediate_mode_if_input_is_not_8_return_0(self):
        self.assertEqual(intcode([3,3,1108,-1,8,3,4,3,99], 3).output[0], 0)

    def test_intcode_immediate_mode_if_input_is_less_than_8_return_1(self):
        self.assertEqual(intcode([3,3,1107,-1,8,3,4,3,99], 7).output[0], 1)
    def test_intcode_immediate_mode_if_input_is_not_less_than_8_return_0(self):
        self.assertEqual(intcode([3,3,1107,-1,8,3,4,3,99], 8).output[0], 0)


    def test_intcode_position_mode_check_if_input_is_not_zero_return_1(self):
        self.assertEqual(intcode([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 2).output[0], 1)
    def test_intcode_position_mode_check_if_input_is_zero_return_0(self):
        self.assertEqual(intcode([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 0).output[0], 0)

    def test_intcode_immediate_mode_check_if_input_is_not_zero_return_1(self):
        self.assertEqual(intcode([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 2).output[0], 1)
    def test_intcode_immediate_mode_check_if_input_is_zero_return_0(self):
        self.assertEqual(intcode([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 0).output[0], 0)

    def test_intcode_if_input_is_below_8(self):
        self.assertEqual(intcode([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 7).output[0], 999)
    def test_intcode_if_input_is__8(self):
        self.assertEqual(intcode([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 8).output[0], 1000)
    def test_intcode_if_input_is_above_8(self):
        self.assertEqual(intcode([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 9).output[0], 1001)



## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 5")
    print("Part1 result: {}".format(part1(getCommaSeparatedIntsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getCommaSeparatedIntsFromFile(sys.argv[1]))))

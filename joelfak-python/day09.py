#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
import operator, itertools, math
from collections import namedtuple, deque

IntcodeResult = namedtuple('IntcodeResult', ['programDone', 'output'])
OpCode = namedtuple('OpCode', ['opcode','parameterModes'])

def parseOpcode(opcodeIn):
    parameterModes = deque()
    opcode = opcodeIn % 100
    opcodeIn //= 100
    while opcodeIn:
        parameterModes.append(opcodeIn % 10)
        opcodeIn //= 10

    return OpCode(opcode, parameterModes)

def printIfVerbose(str, verbose):
    if verbose:
        print(str)

class IncodeComputer:
    def __init__(self, program, name="IntcodeComputer", verbose=False):
        self.program = program.copy()
        self.highestPosition = 0
        self.name = name
        self.verbose = verbose
        self.p = 0
        self.relativeBase = 0

    def getUsedProgram(self):
        return self.program[0:self.highestPosition+1]

    def printIfVerbose(self, str):
        printIfVerbose(str, self.verbose)

    def readMemory(self, position):
        if position >= len(self.program):
            self.program.extend([0] * 100)
        if position > self.highestPosition:
            self.highestPosition = position
        return self.program[position]

    def writeMemory(self, position, parameter):
        if position >= len(self.program):
            self.program.extend([0] * 100)
        if position > self.highestPosition:
            self.highestPosition = position
        self.program[position] = parameter

    def getParameter(self, pointerOffset, parameterModes):
        if len(parameterModes) > 0:
            paramMode = parameterModes.popleft()
            self.printIfVerbose("paramMode: {}".format(paramMode))
            if paramMode == 1: # Immediate mode
                self.printIfVerbose("Immediate mode")
                return self.readMemory(self.p+pointerOffset)
            if paramMode == 2: # Relative mode
                self.printIfVerbose("Relative mode")
                return self.readMemory(self.relativeBase + self.readMemory(self.p+pointerOffset))
        # Position mode
        self.printIfVerbose("Position mode")
        return self.readMemory(self.readMemory(self.p+pointerOffset))

    def increaseProgramPointer(self, steps):
        self.p += steps

    def printInstruction(self, descriptionFormat, numParams = 3):
        params = [self.readMemory(self.p + 1 + x) for x in range(numParams)]
        self.printIfVerbose(descriptionFormat.format(*params))

    def runUntilHalt(self, input=None):
        oc = parseOpcode(self.readMemory(self.p))

        programDone = False
        output = []

        # print("{} program: {}".format(self.name, self.program))

        self.printIfVerbose(self.program)
        while oc.opcode != 99:
            self.printIfVerbose("\nopcode: {} parameterModes: {}".format(oc.opcode, list(oc.parameterModes)))
            if oc.opcode == 1: # Addition
                self.printInstruction("Addition, parameters: {}, {}, {}", 3)
                param1 = self.getParameter(1, oc.parameterModes)
                param2 = self.getParameter(2, oc.parameterModes)
                self.writeMemory(self.readMemory(self.p+3), param1 + param2)
                self.printIfVerbose("Add: {} + {} = {}".format(param1, param2, param1+param2))
                self.increaseProgramPointer(4)

            elif oc.opcode == 2: # Multiplication
                self.printInstruction("Multiplication, parameters: {}, {}, {}", 3)
                self.writeMemory(self.readMemory(self.p+3), self.getParameter(1, oc.parameterModes) * self.getParameter(2, oc.parameterModes))
                self.increaseProgramPointer(4)

            elif oc.opcode == 3: # Read input
                self.printInstruction("Read input", 0)
                if len(input) == 0:
                    programDone = False
                    self.printIfVerbose("{} waiting for input".format(self.name))
                    break
                self.printIfVerbose("{} reading input {}".format(self.name, input[0]))
                self.writeMemory(self.readMemory(self.p+1), input.pop(0))
                self.increaseProgramPointer(2)

            elif oc.opcode == 4: # Write output
                self.printInstruction("Write output, parameters: {}", 1)
                output.append(self.getParameter(1, oc.parameterModes))
                self.printIfVerbose("{} writing output {}".format(self.name, output[len(output)-1]))
                self.increaseProgramPointer(2)

            elif oc.opcode == 5: # Jump if true
                self.printInstruction("Jump if true, parameters: {}, {}", 2)
                if self.getParameter(1, oc.parameterModes) != 0:
                    self.p = self.getParameter(2, oc.parameterModes)
                else:
                    self.increaseProgramPointer(3)

            elif oc.opcode == 6: # Jump if false
                self.printInstruction("Jump if false, parameters: {}, {}", 2)
                if self.getParameter(1, oc.parameterModes) == 0:
                    self.p = self.getParameter(2, oc.parameterModes)
                else:
                    self.increaseProgramPointer(3)

            elif oc.opcode == 7: # Less than
                self.printInstruction("Less than, parameters: {}, {}, {}", 3)
                if self.getParameter(1, oc.parameterModes) < self.getParameter(2, oc.parameterModes):
                    self.writeMemory(self.readMemory(self.p+3), 1)
                else:
                    self.writeMemory(self.readMemory(self.p+3), 0)
                self.increaseProgramPointer(4)

            elif oc.opcode == 8: # Equals
                self.printInstruction("Equals, parameters: {}, {}, {}", 3)
                if self.getParameter(1, oc.parameterModes) == self.getParameter(2, oc.parameterModes):
                    self.writeMemory(self.readMemory(self.p+3), 1)
                else:
                    self.writeMemory(self.readMemory(self.p+3), 0)
                self.increaseProgramPointer(4)

            elif oc.opcode == 9: # Move relative base
                self.printInstruction("Move relative base, parameters: {}", 1)
                parameter = self.getParameter(1, oc.parameterModes)
                self.printIfVerbose("move base - current: {}, parameter: {}".format(self.relativeBase, parameter))
                self.relativeBase += parameter
                self.printIfVerbose("new base: {}".format(self.relativeBase))
                self.increaseProgramPointer(2)

            else:
                raise Exception('Invalid operator', oc.opcode)

            oc = parseOpcode(self.readMemory(self.p))
            if oc.opcode == 99:
                programDone = True

        return IntcodeResult(programDone, output)

def part1(program):
    return IncodeComputer(program).runUntilHalt([1]).output

def part2(program):
    return findBestAmplifierPhaseSettings(program, range(5,10))[1]

## Unit tests ########################################################

class TestDay02(unittest.TestCase):
    def test_intcode_example_program_1(self):
        ic = IncodeComputer([1,0,0,0,99])
        ic.runUntilHalt()
        self.assertEqual(ic.getUsedProgram(), [2,0,0,0,99])

    def test_intcode_example_program_2(self):
        ic = IncodeComputer([2,3,0,3,99])
        ic.runUntilHalt()
        self.assertEqual(ic.getUsedProgram(), [2,3,0,6,99])

    def test_intcode_example_program_3(self):
        ic = IncodeComputer([2,4,4,5,99,0])
        ic.runUntilHalt()
        self.assertEqual(ic.getUsedProgram(), [2,4,4,5,99,9801])

    def test_intcode_example_program_4(self):
        ic = IncodeComputer([1,1,1,4,99,5,6,0,99])
        ic.runUntilHalt()
        self.assertEqual(ic.getUsedProgram(), [30,1,1,4,2,5,6,0,99])

    def test_intcode_example_program(self):
        ic = IncodeComputer([1,9,10,3,2,3,11,0,99,30,40,50])
        ic.runUntilHalt()
        self.assertEqual(ic.getUsedProgram(), [3500,9,10,70,2,3,11,0,99,30,40,50])

class TestParseOpcode(unittest.TestCase):
    def test_1(self):
        self.assertEqual(parseOpcode(1002), (2, deque([0, 1])))

    def test_2(self):
        self.assertEqual(parseOpcode(101003), (3, deque([0, 1, 0, 1])))

class TestDay05_part1(unittest.TestCase):
    def test_intcode_output_is_input_1(self):
        self.assertEqual(IncodeComputer([3,0,4,0,99]).runUntilHalt([3]).output[0], 3)

    def test_intcode_output_is_input_2(self):
        self.assertEqual(IncodeComputer([3,0,4,0,99]).runUntilHalt([7]).output[0], 7)

    def test_intcode_parameter_mode(self):
        ic = IncodeComputer([1002,4,3,4,33])
        ic.runUntilHalt()
        self.assertEqual(ic.getUsedProgram(), [1002,4,3,4,99])

    def test_intcode_negative_numbers(self):
        ic = IncodeComputer([1101,100,-1,4,0])
        ic.runUntilHalt()
        self.assertEqual(ic.getUsedProgram(), [1101,100,-1,4,99])

class TestDay05_part2(unittest.TestCase):
    def test_intcode_if_input_is_8_return_1(self):
        self.assertEqual(IncodeComputer([3,9,8,9,10,9,4,9,99,-1,8]).runUntilHalt([8]).output[0], 1)
    def test_intcode_if_input_is_not_8_return_0(self):
        self.assertEqual(IncodeComputer([3,9,8,9,10,9,4,9,99,-1,8]).runUntilHalt([3]).output[0], 0)

    def test_intcode_if_input_is_less_than_8_return_1(self):
        self.assertEqual(IncodeComputer([3,9,7,9,10,9,4,9,99,-1,8]).runUntilHalt([7]).output[0], 1)
    def test_intcode_if_input_is_not_less_than_8_return_0(self):
        self.assertEqual(IncodeComputer([3,9,7,9,10,9,4,9,99,-1,8]).runUntilHalt([8]).output[0], 0)

    def test_intcode_immediate_mode_if_input_is_8_return_1(self):
        self.assertEqual(IncodeComputer([3,3,1108,-1,8,3,4,3,99]).runUntilHalt([8]).output[0], 1)
    def test_intcode_immediate_mode_if_input_is_not_8_return_0(self):
        self.assertEqual(IncodeComputer([3,3,1108,-1,8,3,4,3,99]).runUntilHalt([3]).output[0], 0)

    def test_intcode_immediate_mode_if_input_is_less_than_8_return_1(self):
        self.assertEqual(IncodeComputer([3,3,1107,-1,8,3,4,3,99]).runUntilHalt([7]).output[0], 1)
    def test_intcode_immediate_mode_if_input_is_not_less_than_8_return_0(self):
        self.assertEqual(IncodeComputer([3,3,1107,-1,8,3,4,3,99]).runUntilHalt([8]).output[0], 0)


    def test_intcode_position_mode_check_if_input_is_not_zero_return_1(self):
        self.assertEqual(IncodeComputer([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]).runUntilHalt([2]).output[0], 1)
    def test_intcode_position_mode_check_if_input_is_zero_return_0(self):
        self.assertEqual(IncodeComputer([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]).runUntilHalt([0]).output[0], 0)

    def test_intcode_immediate_mode_check_if_input_is_not_zero_return_1(self):
        self.assertEqual(IncodeComputer([3,3,1105,-1,9,1101,0,0,12,4,12,99,1]).runUntilHalt([2]).output[0], 1)
    def test_intcode_immediate_mode_check_if_input_is_zero_return_0(self):
        self.assertEqual(IncodeComputer([3,3,1105,-1,9,1101,0,0,12,4,12,99,1]).runUntilHalt([0]).output[0], 0)

    def test_intcode_if_input_is_below_8(self):
        self.assertEqual(IncodeComputer([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]).runUntilHalt([7]).output[0], 999)
    def test_intcode_if_input_is__8(self):
        self.assertEqual(IncodeComputer([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]).runUntilHalt([8]).output[0], 1000)
    def test_intcode_if_input_is_above_8(self):
        self.assertEqual(IncodeComputer([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]).runUntilHalt([9]).output[0], 1001)

class TestDay09_part1(unittest.TestCase):
    def test_self_copy_program(self):
        ic = IncodeComputer([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
        self.assertEqual(ic.runUntilHalt().output, [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

    def test_output_16_digit_number(self):
        ic = IncodeComputer([1102,34915192,34915192,7,4,7,99,0])
        res = ic.runUntilHalt()
        print("output: {}".format(res.output))
        numDigits = int(math.log10(res.output[0]))+1
        self.assertEqual(numDigits, 16)

    def test_large_number(self):
        ic = IncodeComputer([104,1125899906842624,99])
        res = ic.runUntilHalt()
        self.assertEqual(res.output[0], 1125899906842624)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 9")
    print("Part1 result: {}".format(part1(getCommaSeparatedIntsFromFile(sys.argv[1]))))
    # print("Part2 result: {}".format(part2(getCommaSeparatedIntsFromFile(sys.argv[1]))))

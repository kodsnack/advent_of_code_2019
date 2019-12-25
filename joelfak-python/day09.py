#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
import operator, itertools
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
        self.name = name
        self.verbose = verbose
        self.p = 0

    def printIfVerbose(self, str):
        printIfVerbose(str, self.verbose)

    def getParameter(self, pointerOffset, parameterModes):
        if len(parameterModes) > 0 and parameterModes.popleft():
            return self.program[self.p+pointerOffset]
        else:
            return self.program[self.program[self.p+pointerOffset]]

    def increaseProgramPointer(self, steps):
        self.p += steps

    def runUntilHalt(self, input=None):
        oc = parseOpcode(self.program[self.p])

        programDone = False
        output = []

        # print("{} program: {}".format(self.name, self.program))

        while oc.opcode != 99:
            if oc.opcode == 1: # Addition
                self.program[self.program[self.p+3]] = self.getParameter(1, oc.parameterModes) + self.getParameter(2, oc.parameterModes)
                self.increaseProgramPointer(4)

            elif oc.opcode == 2: # Multiplication
                self.program[self.program[self.p+3]] = self.getParameter(1, oc.parameterModes) * self.getParameter(2, oc.parameterModes)
                self.increaseProgramPointer(4)

            elif oc.opcode == 3: # Read input
                if len(input) == 0:
                    programDone = False
                    self.printIfVerbose("{} waiting for input".format(self.name))
                    break
                self.printIfVerbose("{} reading input {}".format(self.name, input[0]))
                self.program[self.program[self.p+1]] = input.pop(0)
                self.increaseProgramPointer(2)

            elif oc.opcode == 4: # Write output
                output.append(self.getParameter(1, oc.parameterModes))
                self.printIfVerbose("{} writing output {}".format(self.name, output[len(output)-1]))
                self.increaseProgramPointer(2)

            elif oc.opcode == 5: # Jump if true
                if self.getParameter(1, oc.parameterModes) != 0:
                    self.p = self.getParameter(2, oc.parameterModes)
                else:
                    self.increaseProgramPointer(3)

            elif oc.opcode == 6: # Jump if false
                if self.getParameter(1, oc.parameterModes) == 0:
                    self.p = self.getParameter(2, oc.parameterModes)
                else:
                    self.increaseProgramPointer(3)

            elif oc.opcode == 7: # Less than
                if self.getParameter(1, oc.parameterModes) < self.getParameter(2, oc.parameterModes):
                    self.program[self.program[self.p+3]] = 1
                else:
                    self.program[self.program[self.p+3]] = 0
                self.increaseProgramPointer(4)

            elif oc.opcode == 8: # Equals
                if self.getParameter(1, oc.parameterModes) == self.getParameter(2, oc.parameterModes):
                    self.program[self.program[self.p+3]] = 1
                else:
                    self.program[self.program[self.p+3]] = 0
                self.increaseProgramPointer(4)

            else:
                raise Exception('Invalid operator', oc.opcode)

            oc = parseOpcode(self.program[self.p])
            if oc.opcode == 99:
                programDone = True

        return IntcodeResult(programDone, output)

def runAmplifiers(program, phaseSettings, verbose=False):
    printIfVerbose("runAmplifiers with phase settings: {}".format(phaseSettings), verbose)
    ampA = IncodeComputer(program, "ampA", verbose)
    ampB = IncodeComputer(program, "ampB", verbose)
    ampC = IncodeComputer(program, "ampC", verbose)
    ampD = IncodeComputer(program, "ampD", verbose)
    ampE = IncodeComputer(program, "ampE", verbose)

    programDone = 0
    signal = 0

    inputA = [phaseSettings[0]]
    inputB = [phaseSettings[1]]
    inputC = [phaseSettings[2]]
    inputD = [phaseSettings[3]]
    inputE = [phaseSettings[4]]
    inputA.append(0)

    while programDone == False:
        printIfVerbose("inputA: {}".format(inputA), verbose)
        res = ampA.runUntilHalt(inputA)
        if len(res.output) > 0:
            inputB.append(res.output[0])

        printIfVerbose("inputB: {}".format(inputB), verbose)
        res = ampB.runUntilHalt(inputB)
        if len(res.output) > 0:
            inputC.append(res.output[0])

        printIfVerbose("inputC: {}".format(inputC), verbose)
        res = ampC.runUntilHalt(inputC)
        if len(res.output) > 0:
            inputD.append(res.output[0])

        printIfVerbose("inputD: {}".format(inputD), verbose)
        res = ampD.runUntilHalt(inputD)
        if len(res.output) > 0:
            inputE.append(res.output[0])

        printIfVerbose("inputE: {}".format(inputE), verbose)
        res = ampE.runUntilHalt(inputE)
        if len(res.output) > 0:
            inputA.append(res.output[0])

        programDone = res.programDone

    return res.output[0]

def findBestAmplifierPhaseSettings(program, startingParameters):
    bestParams = max(itertools.permutations(startingParameters), key=lambda x: runAmplifiers(program, x))
    return (list(bestParams), runAmplifiers(program, bestParams))

def part1(program):
    return findBestAmplifierPhaseSettings(program, range(0,5))[1]

def part2(program):
    return findBestAmplifierPhaseSettings(program, range(5,10))[1]

## Unit tests ########################################################

class TestDay02(unittest.TestCase):
    def test_intcode_example_program_1(self):
        ic = IncodeComputer([1,0,0,0,99])
        ic.runUntilHalt()
        self.assertEqual(ic.program, [2,0,0,0,99])

    def test_intcode_example_program_2(self):
        ic = IncodeComputer([2,3,0,3,99])
        ic.runUntilHalt()
        self.assertEqual(ic.program, [2,3,0,6,99])

    def test_intcode_example_program_3(self):
        ic = IncodeComputer([2,4,4,5,99,0])
        ic.runUntilHalt()
        self.assertEqual(ic.program, [2,4,4,5,99,9801])

    def test_intcode_example_program_4(self):
        ic = IncodeComputer([1,1,1,4,99,5,6,0,99])
        ic.runUntilHalt()
        self.assertEqual(ic.program, [30,1,1,4,2,5,6,0,99])

    def test_intcode_example_program(self):
        ic = IncodeComputer([1,9,10,3,2,3,11,0,99,30,40,50])
        ic.runUntilHalt()
        self.assertEqual(ic.program, [3500,9,10,70,2,3,11,0,99,30,40,50])

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
        self.assertEqual(ic.program, [1002,4,3,4,99])

    def test_intcode_negative_numbers(self):
        ic = IncodeComputer([1101,100,-1,4,0])
        ic.runUntilHalt()
        self.assertEqual(ic.program, [1101,100,-1,4,99])

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


class TestDay07_part1(unittest.TestCase):
    def test_example_program_1(self):
        self.assertEqual(runAmplifiers([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0],
                                        [4,3,2,1,0]),
                                        43210)

    def test_example_program_2(self):
        self.assertEqual(runAmplifiers([3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                        101,5,23,23,1,24,23,23,4,23,99,0,0],
                                        [0,1,2,3,4]),
                                        54321)

    def test_example_program_3(self):
        self.assertEqual(runAmplifiers([3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                        1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0],
                                        [1,0,4,3,2]),
                                        65210)

    def test_findBestAmplifierPhaseSettings_example_program_1(self):
        self.assertEqual(findBestAmplifierPhaseSettings([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0],
                                                        [0,1,2,3,4]),
                                                        ([4,3,2,1,0], 43210))

    def test_findBestAmplifierPhaseSettings_example_program_2(self):
        self.assertEqual(findBestAmplifierPhaseSettings([3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                                        101,5,23,23,1,24,23,23,4,23,99,0,0],
                                                        [0,1,2,3,4]),
                                                        ([0,1,2,3,4], 54321))

    def test_findBestAmplifierPhaseSettings_example_program_3(self):
        self.assertEqual(findBestAmplifierPhaseSettings([3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                                        1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0],
                                                        [0,1,2,3,4]),
                                                        ([1,0,4,3,2], 65210))

class TestDay07_part2(unittest.TestCase):
    def test_part2_example_program_1(self):
        self.assertEqual(runAmplifiers([3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                                        27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5],
                                        [9,8,7,6,5]),
                                        139629729)

    def test_part2_example_program_2(self):
        self.assertEqual(runAmplifiers([3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                                        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                                        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10],
                                        [9,7,8,5,6]),
                                        18216)

    def test_part2_findBestAmplifierPhaseSettings_example_program_1(self):
        self.assertEqual(findBestAmplifierPhaseSettings([3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                                                        27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5],
                                                        [5,6,7,8,9]),
                                                        ([9,8,7,6,5], 139629729))

    def test_part2_findBestAmplifierPhaseSettings_example_program_1(self):
        self.assertEqual(findBestAmplifierPhaseSettings([3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                                                        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                                                        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10],
                                                        [5,6,7,8,9])[1],    # finds solution    [9, 7, 8, 5, 6]
                                                        18216)              # which is equal to [9, 8, 7, 6, 5]

class TestDay09_part1(unittest.TestCase):
    def test_self_copy_program(self):
        print()
        ic = IncodeComputer([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], verbose=True)
        ic.runUntilHalt()
        self.assertEqual(ic.program, [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 7")
    print("Part1 result: {}".format(part1(getCommaSeparatedIntsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getCommaSeparatedIntsFromFile(sys.argv[1]))))

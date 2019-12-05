import day05
import unittest

const
    sol1 = "0\n0\n0\n0\n0\n0\n0\n0\n0\n12440243\n"
    sol2 = "15486302\n"

suite "day05, problem 1":
    test "example 1":
        const
            input1 = """1002,4,3,4,33"""
        var
            memory = loadMemory(input1)
        discard run(memory)
        check(memory[4] == 99)
    test "example 2":
        const
            input2 = """3,0,4,0,99"""
            userInput = """2
"""
        var
            memory = loadMemory(input2)
        let
            output = run(memory, false, userInput)
        check(output == userInput)
    test "example 3":
        const
            input3 = """1101,100,-1,4,0"""
        var
            memory = loadMemory(input3)
        discard run(memory)
        check(memory[4] == 99)
    test "solution":
        let
            input = open("inputs/day05.txt").readAll()
        const
            userInput = """1"""
        var
            memory = loadMemory(input)
        let
            output = run(memory, false, userInput)
        check(output == sol1)
        echo("Solution 1: ", sol1)

suite "day05, problem 2":
    test "example 1":
        const
            input1 = "3,9,8,9,10,9,4,9,99,-1,8"
            expected1 = "0\n"
            userInput = """7"""
        var
            memory = loadMemory(input1)
        check(expected1 == run(memory, false, userInput))
    test "example 2":
        const
            input2 = "3,9,7,9,10,9,4,9,99,-1,8"
            expected2 = "1\n"
            userInput = """7"""
        var
            memory = loadMemory(input2)
        check(expected2 == run(memory, false, userInput))
    test "solution":
        let
            input = open("inputs/day05.txt").readAll()
            userInput = """5"""
        var
            memory = loadMemory(input)
        check(sol2 == run(memory, false, userInput))
        echo("Solution 2: ", sol2)        
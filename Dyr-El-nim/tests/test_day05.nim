import day05
import unittest

const
    sol1 = 4945026
    sol2 = 5296

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
            userInput = """2"""
        var
            memory = loadMemory(input2)
        let
            output = run(memory, userInput)
        check(output == userInput)

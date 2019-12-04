import day02
import unittest

const
    sol1 = 4945026
    sol2 = 5296

suite "day02, problem 1":
    test "example 1":
        const
            input1 = """1,9,10,3,2,3,11,0,99,30,40,50"""
        var
            memory = loadMemory(input1)
        run(memory)
        check(memory[0] == 3500)
    test "example 2":
        const
            input2 = """1,0,0,0,99"""
        var
            memory = loadMemory(input2)
        run(memory)
        check(memory[0] == 2)
    test "example 3":
        const
            input3 = """2,3,0,3,99"""
        var
            memory = loadMemory(input3)
        run(memory)
        check(memory[3] == 6)
    test "example 4":
        const
            input4 = """2,4,4,5,99,0"""
        var
            memory = loadMemory(input4)
        run(memory)
        check(memory[5] == 9801)
    test "example 5":
        const
            input5 = """1,1,1,4,99,5,6,0,99"""
        var
            memory = loadMemory(input5)
        run(memory)
        check(memory[0] == 30)
    test "solution":
        let
            input = open("inputs/day02.txt").readAll()
        var
            memory = loadMemory(input)
        memory.setNoun(12)
        memory.setVerb(2)
        run(memory)
        check(memory[0] == sol1)
        echo("Solution 1: ", sol1)

suite "day02, problem 2":
    test "solution":
        let
            input = open("inputs/day02.txt").readAll()
        check(findNounAndVerb(input, 19690720) == sol2)
        echo("Solution 2: ", sol2)

import day03
import unittest

const
    sol1 = 8015
    sol2 = 163676
    input1 = """R8,U5,L5,D3
U7,R6,D4,L4"""
    input2 = """R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"""
    input3 = """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"""

suite "day03, problem 1":
    test "example 1":
        check(findMinDistance(input1) == 6)
    test "example 2":
        check(findMinDistance(input2) == 159)
    test "example 3":
        check(findMinDistance(input3) == 135)
    test "solution":
        let
            input = open("inputs/day03.txt").readAll()
        check(findMinDistance(input) == sol1)
        echo("Solution 1: ", sol1)        
suite "day03, problem 2":
    test "example 1":
        check(findMinSteps(input1) == 30)
    test "example 2":
        check(findMinSteps(input2) == 610)
    test "example 3":
        check(findMinSteps(input3) == 410)
    test "solution":
        let
            input = open("inputs/day03.txt").readAll()
        check(findMinSteps(input) == sol2)
        echo("Solution 2: ", sol2)        

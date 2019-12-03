import day03
import unittest

const
    sol1 = 4945026
    sol2 = 5296
    input1 = """R8,U5,L5,D3
U7,R6,D4,L4"""

suite "day03, problem 1":
    test "example 1":
        check(findMinDistance(input1) == 6)

suite "day03, problem 2":
    test "example 1":
        discard
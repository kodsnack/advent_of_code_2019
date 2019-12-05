import day04
import unittest

const
    sol1 = 1716
    sol2 = 1163

suite "day04, problem 1":
    test "example 1":
        check(test1(111111))
    test "example 2":
        check(not test1(223450))
    test "example 3":
        check(not test1(123789))
    test "solution":
        check(countPasswords(165432, 707912, test1) == sol1)
        echo("Solution 1: ", sol1)        


suite "day04, problem 2":
    test "example 1":
        check(test2(112233))
    test "example 2":
        check(not test2(123444))
    test "example 3":
        check(test2(111122))
    test "solution":
        check(countPasswords(165432, 707912, test2) == sol2)
        echo("Solution 2: ", sol2)        
    
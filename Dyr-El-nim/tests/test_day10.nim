import day10
import unittest

const
    sol1 = 292
    sol2 = 317

suite "day10, problem 1":
    test "example 1":
        let
            input = """.#..#
            .....
            #####
            ....#
            ...##"""
            expected = 8
        check(findBestAsteroid(input) == expected)
    # test "solution":
    #     let
    #         input = open("inputs/day09.txt").readAll()
    #         expected:seq[Value] = @[sol1]
    #     check(runProgram(input, @[1'i64]) == expected)
    #     echo("Solution 1: ", sol1)
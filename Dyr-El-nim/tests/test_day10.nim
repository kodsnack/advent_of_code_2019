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
    test "solution":
        let
            input = open("inputs/day10.txt").readAll()
        check(findBestAsteroid(input) == sol1)
        echo("Solution 1: ", sol1)
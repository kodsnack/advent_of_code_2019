import day07
import unittest

const
    sol1 = 65464
    sol2 = 1518124

suite "day07, problem 1":
    test "example 1":
        let
            input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        check(optimiseThrusters(input) == 43210)
    test "solution":
        let
            input = open("inputs/day07.txt").readAll()
        check(optimiseThrusters(input) == sol1)
        echo("Solution 1: ", sol1)

suite "day07, problem 2":
    test "solution":
        let
            input = open("inputs/day07.txt").readAll()
        check(optimiseThrustersFeedback(input) == sol2)
        echo("Solution 1: ", sol2)
        
import day01
import unittest

const
    sol1 = 3369286
    sol2 = 5051054

suite "day01, problem 1":
    test "example 1":
        check(fuelForMass(12) == 2)
    test "example 2":
        check(fuelForMass(14) == 2)
    test "example 3":
        check(fuelForMass(1969) == 654)
    test "example 4":
        check(fuelForMass(100756) == 33583)
    test "solution":
        let
            inFile = open("inputs/day01.txt")
        check(solution1(inFile) == sol1)
        echo("Solution 1: ", sol1)
        inFile.close()

suite "day01, problem 2":
    test "example 1":
        check(fuelForAllMass(12) == 2)
    test "example 2":
        check(fuelForAllMass(14) == 2)
    test "example 3":
        check(fuelForAllMass(1969) == 966)
    test "example 4":
        check(fuelForAllMass(100756) == 50346)
    test "solution":
        let
            inFile = open("inputs/day01.txt")
        check(solution2(inFile) == sol2)
        echo("Solution 2: ", sol2)
        inFile.close()
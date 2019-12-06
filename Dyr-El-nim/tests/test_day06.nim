import day06
import unittest

const
    sol1 = 171213
    sol2 = 292

suite "day06, problem 1":
    test "example 1":
        const
            input1 = """COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L"""
        check(totalOrbits(input1) == 42)
    test "solution":
        let
            input = open("inputs/day06.txt").readAll()
        check(totalOrbits(input) == sol1)
        echo("Solution 1: ", sol1)

suite "day06, problem 2":
    test "example 1":
        const
            input1 = """COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L
            K)YOU
            I)SAN"""
        check(minOrbitTrans(input1, "YOU", "SAN") == 4)
    test "solution":
        let
            input = open("inputs/day06.txt").readAll()
        check(minOrbitTrans(input, "YOU", "SAN") == sol2)
        echo("Solution 1: ", sol2)
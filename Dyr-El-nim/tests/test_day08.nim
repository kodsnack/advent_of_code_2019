import day08
import unittest

const
    sol1 = 2286
    sol2 = ("\n XX    XX XXXX X    XXX  " &
            "\nX  X    X    X X    X  X " &
            "\nX       X   X  X    X  X " &
            "\nX       X  X   X    XXX  " &
            "\nX  X X  X X    X    X    " &
            "\n XX   XX  XXXX XXXX X    ")

suite "day08, problem 1":
    test "example 1":
        let
            input = "123456789012"
            width = 3
            height = 2
        check(checkProduct(input, width, height) == 1)
    test "solution":
        let
            input = open("inputs/day08.txt").readAll()
            width = 25
            height = 6
        check(checkProduct(input, width, height) == sol1)
        echo("Solution 1: ", sol1)
        
suite "day08, problem 2":
    test "example 1":
        let
            input = "0222112222120000"
            width = 2
            height = 2
            expected = "\n X\nX "
        check(drawImage(input, width, height) == expected)
    test "solution":
        let
            input = open("inputs/day08.txt").readAll()
            width = 25
            height = 6
        check(drawImage(input, width, height).len == sol2.len)
        check(drawImage(input, width, height) == sol2)
        echo("Solution 2: ", sol2)
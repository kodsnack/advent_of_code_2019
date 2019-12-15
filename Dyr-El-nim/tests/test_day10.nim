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
    test "example 2":
        let
            input = """......#.#.
            #..#.#....
            ..#######.
            .#.#.###..
            .#..#.....
            ..#....#.#
            #..#....#.
            .##.#..###
            ##...#..#.
            .#....####"""
            expected = 33
        check(findBestAsteroid(input) == expected)
    test "example 3":
        let
            input = """#.#...#.#.
            .###....#.
            .#....#...
            ##.#.#.#.#
            ....#.#.#.
            .##..###.#
            ..#...##..
            ..##....##
            ......#...
            .####.###."""
            expected = 35
        check(findBestAsteroid(input) == expected)
    test "example 4":
        let
            input = """.#..#..###
            ####.###.#
            ....###.#.
            ..###.##.#
            ##.##.#.#.
            ....###..#
            ..#.#..#.#
            #..#.#.###
            .##...##.#
            .....#.#.."""
            expected = 41
        check(findBestAsteroid(input) == expected)
    test "example 5":
        let
            input = """.#..##.###...#######
            ##.############..##.
            .#.######.########.#
            .###.#######.####.#.
            #####.##.#.##.###.##
            ..#####..#.#########
            ####################
            #.####....###.#.#.##
            ##.#################
            #####.##.###..####..
            ..######..##.#######
            ####.##.####...##..#
            .#####..#.######.###
            ##...#.##########...
            #.##########.#######
            .####.#.###.###.#.##
            ....##.##.###..#####
            .#.#.###########.###
            #.#.#.#####.####.###
            ###.##.####.##.#..##"""
            expected = 210
        check(findBestAsteroid(input) == expected)
    test "solution":
        let
            input = open("inputs/day10.txt").readAll()
        check(findBestAsteroid(input) == sol1)
        echo("Solution 1: ", sol1)
suite "day10, problem 2":
    setup:
        let
            exinput = """.#..##.###...#######
            ##.############..##.
            .#.######.########.#
            .###.#######.####.#.
            #####.##.#.##.###.##
            ..#####..#.#########
            ####################
            #.####....###.#.#.##
            ##.#################
            #####.##.###..####..
            ..######..##.#######
            ####.##.####...##..#
            .#####..#.######.###
            ##...#.##########...
            #.##########.#######
            .####.#.###.###.#.##
            ....##.##.###..#####
            .#.#.###########.###
            #.#.#.#####.####.###
            ###.##.####.##.#..##"""
    test "example 1":
        check(numberVaporised(exinput, 1) == 1112)
    test "example 2":
        check(numberVaporised(exinput, 2) == 1201)
    test "example 3":
        check(numberVaporised(exinput, 3) == 1202)
    test "example 4":
        check(numberVaporised(exinput, 10) == 1208)
    test "example 5":
        check(numberVaporised(exinput, 20) == 1600)
    test "example 6":
        check(numberVaporised(exinput, 50) == 1609)
    test "example 7":
        check(numberVaporised(exinput, 100) == 1016)
    test "example 8":
        check(numberVaporised(exinput, 199) == 906)
    test "example 9":
        check(numberVaporised(exinput, 200) == 802)
    test "example 10":
        check(numberVaporised(exinput, 201) == 1009)
    test "example 11":
        check(numberVaporised(exinput, 299) == 1101)
    test "solution":
        let
            input = open("inputs/day10.txt").readAll()
        check(numberVaporised(input, 200) == sol2)
        echo("Solution 2: ", sol2)

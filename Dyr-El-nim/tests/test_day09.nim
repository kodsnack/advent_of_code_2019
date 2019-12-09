import day09
import unittest

const
    sol1 = 3409270027'i64
    sol2 = 82760'i64

suite "day09, problem 1":
    test "example 1":
        let
            input = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
            expected:seq[Value] = @[109'i64,1'i64,204'i64,-1'i64,1001'i64,100'i64,1'i64,100'i64,1008'i64,
                                    100'i64,16'i64,101'i64,1006'i64,101'i64,0'i64,99'i64]
        check(runProgram(input, @[]) == expected)
    test "example 2":
        let
            input = "1102,34915192,34915192,7,4,7,99,0"
            expected:seq[Value] = @[1219070632396864'i64]
        check(runProgram(input, @[]) == expected)
    test "example 3":
        let
            input = "104,1125899906842624,99"
            expected:seq[Value] = @[1125899906842624'i64]
        check(runProgram(input, @[]) == expected)
    test "solution":
        let
            input = open("inputs/day09.txt").readAll()
            expected:seq[Value] = @[sol1]
        check(runProgram(input, @[1'i64]) == expected)
        echo("Solution 1: ", sol1)
        
suite "day08, problem 2":
    test "solution":
        let
            input = open("inputs/day09.txt").readAll()
            expected:seq[Value] = @[sol2]
        check(runProgram(input, @[2'i64]) == expected)
        echo("Solution 2: ", sol2)

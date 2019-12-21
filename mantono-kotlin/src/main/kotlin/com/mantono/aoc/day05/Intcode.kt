package com.mantono.aoc.day05

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import com.mantono.aoc.runProgram

@AoC(5, Part.A)
fun intCode(input: String): Int {
    return runProgram(input, 1)
}

@AoC(5, Part.B)
fun intCodeExtended(input: String): Int {
    return runProgram(input, 5)
}
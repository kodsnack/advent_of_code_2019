package com.mantono.aoc.day05

import com.mantono.aoc.AoC
import com.mantono.aoc.IntCodeComputer
import com.mantono.aoc.Part

@AoC(5, Part.A)
fun intCode(input: String): Int {
    return IntCodeComputer.loadProgram(input).run(1).pop()
}

@AoC(5, Part.B)
fun intCodeExtended(input: String): Int {
    return IntCodeComputer.loadProgram(input).run(6).pop()
}
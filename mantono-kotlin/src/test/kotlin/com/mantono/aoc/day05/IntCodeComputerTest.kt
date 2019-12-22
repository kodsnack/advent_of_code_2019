package com.mantono.aoc

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class IntCodeComputerTest {
    @Test
    fun testModeParsingFourDigits() {
        val code = 1102
        val modes: List<Mode> = Mode.parse(code)
        assertEquals(listOf(Mode.Value, Mode.Value, Mode.Address), modes)
    }

    @Test
    fun testModeParsingFiveDigits() {
        val code = 10002
        val modes: List<Mode> = Mode.parse(code)
        assertEquals(listOf(Mode.Address, Mode.Address, Mode.Value), modes)
    }

    @Test
    fun testJumpProgramWithPositionModeAndZeroInput() {
        val program = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        val input = 0
        val output = IntCodeComputer.loadProgram(program).run(input)
        assertEquals(0, output.pop())
    }

    @Test
    fun testJumpProgramWithPositionModeAndNonZeroInput() {
        val program = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        val input = 5
        val output = IntCodeComputer.loadProgram(program).run(input)
        assertEquals(1, output.pop())
    }

    @Test
    fun testJumpProgramWithImmediateModeAndZeroInput() {
        val program = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        val input = 0
        val output = IntCodeComputer.loadProgram(program).run(input)
        assertEquals(0, output.pop())
    }

    @Test
    fun testJumpProgramWithImmediateModeAndNonZeroInput() {
        val program = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        val input = 5
        val output = IntCodeComputer.loadProgram(program).run(input)
        assertEquals(1, output.pop())
    }

    @Test
    fun testLongExampleEqualToEight() {
        val program = """
            3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        """.trimIndent()
        val input = 8
        val output = IntCodeComputer.loadProgram(program).run(input)
        assertEquals(1000, output.pop())
    }

    @Test
    fun testLongExampleEqualBelowEight() {
        val program = """
            3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        """.trimIndent()
        val input = 7
        val output = IntCodeComputer.loadProgram(program).run(input)
        assertEquals(999, output.pop())
    }

    @Test
    fun testLongExampleEqualAboveEight() {
        val program = """
            3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        """.trimIndent()
        val input = 9
        val output = IntCodeComputer.loadProgram(program).run(input)
        assertEquals(1001, output.pop())
    }
}
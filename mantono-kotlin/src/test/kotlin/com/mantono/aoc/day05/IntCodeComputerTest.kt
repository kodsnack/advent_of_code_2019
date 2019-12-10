package com.mantono.aoc.day05

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class IntCodeComputerTest {
    @Test
    fun testModeParsingFourDigits() {
        val code = 1102
        val modes: List<Mode> = Mode.parse(code)
        assertEquals(listOf(Mode.Address, Mode.Value, Mode.Value, Mode.Address), modes)
    }

    @Test
    fun testModeParsingFiveDigits() {
        val code = 10002
        val modes: List<Mode> = Mode.parse(code)
        assertEquals(listOf(Mode.Address, Mode.Address, Mode.Address, Mode.Value), modes)
    }
}
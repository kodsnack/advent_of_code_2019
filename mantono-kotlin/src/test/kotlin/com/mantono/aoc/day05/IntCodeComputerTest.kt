package com.mantono.aoc.day05

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class IntCodeComputerTest {
    @Test
    fun testOpCodeParsing() {
        val opcode: Pair<OpCode, List<Mode>> = OpCode.parse(1002)
        assertEquals(OpCode.MULT, opcode.first)
        assertEquals(listOf(Mode.Address, Mode.Value, Mode.Address), opcode.second)
    }
}
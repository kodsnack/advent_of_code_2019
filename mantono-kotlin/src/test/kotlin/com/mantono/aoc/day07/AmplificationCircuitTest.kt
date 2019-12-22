package com.mantono.aoc.day07

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class AmplificationCircuitTest {
    @Test
    fun testSequenceGenerationRightOrder() {
        val permutations: List<List<Int>> = phaseSettingsGenerator()
            .take(5)
            .toList()

        assertEquals(listOf(0, 1, 2, 3, 4), permutations[0])
        assertEquals(listOf(0, 1, 2, 4, 3), permutations[1])
        assertEquals(listOf(0, 1, 3, 2, 4), permutations[2])
        assertEquals(listOf(0, 1, 3, 4, 2), permutations[3])
        assertEquals(listOf(0, 1, 4, 2, 3), permutations[4])
    }
}
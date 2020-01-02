package com.mantono.aoc.day07

import com.mantono.aoc.IntCodeComputer
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.util.*

class AmplificationCircuitTest {
    @Test
    fun testSequenceGenerationRightOrder() {
        val permutations: List<Deque<Int>> = phaseSettingsGenerator(listOf(0, 1, 2, 3, 4))
            .take(5)
            .toList()

        assertEquals(listOf(0, 1, 2, 3, 4), permutations[0])
        assertEquals(listOf(0, 1, 2, 4, 3), permutations[1])
        assertEquals(listOf(0, 1, 3, 2, 4), permutations[2])
        assertEquals(listOf(0, 1, 3, 4, 2), permutations[3])
        assertEquals(listOf(0, 1, 4, 2, 3), permutations[4])
    }

    @Test
    fun testProgramWithFeedbackLoop() {
        val program = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
        val maxThrusterSignal: Int = runWithFeedbackLoop(dequeOf(9, 8, 7, 6, 5), program)
        assertEquals(139629729, maxThrusterSignal)
    }
}
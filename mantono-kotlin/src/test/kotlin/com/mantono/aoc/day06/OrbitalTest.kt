package com.mantono.aoc.day06

import com.mantono.aoc.day05.Mode
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class OrbitalTest {

    @Test
    fun computeOrbitsTest() {
        val orbitsInput = """
            COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L
        """.trimIndent()
        assertEquals(42, calculateOrbits(orbitsInput))
    }

    @Test
    fun testOrbitalTransfers() {
        val orbitsInput = """
            COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L
            K)YOU
            I)SAN
        """.trimIndent()
        assertEquals(4, orbitalTransfers(orbitsInput))
    }
}
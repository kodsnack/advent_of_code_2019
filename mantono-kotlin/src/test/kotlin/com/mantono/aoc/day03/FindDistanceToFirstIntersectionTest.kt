package com.mantono.aoc.day03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class FindDistanceToFirstIntersectionTest {

    @Test
    fun testFindDistanceToFirstIntersectionLong() {
        val input: String = """
            R75,D30,R83,U83,L12,D49,R71,U7,L72
            U62,R66,U55,R34,D71,R55,D58,R83
        """.trimIndent()
        val distance: Int = findDistanceToFirstIntersection(input)
        assertEquals(610, distance)
    }

    @Test
    fun testFindDistanceToFirstIntersectionMedium() {
        val input: String = """
            R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
            U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
        """.trimIndent()
        val distance: Int = findDistanceToFirstIntersection(input)
        assertEquals(410, distance)
    }

    @Test
    fun testFindDistanceToFirstIntersectionShort() {
        val input: String = """
            R8,U5,L5,D3
            U7,R6,D4,L4
        """.trimIndent()
        val distance: Int = findDistanceToFirstIntersection(input)
        assertEquals(30, distance)
    }
}
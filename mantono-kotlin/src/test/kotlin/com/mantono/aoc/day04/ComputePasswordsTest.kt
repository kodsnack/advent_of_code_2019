package com.mantono.aoc.day04

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class ComputePasswordsTest {

    @Test
    fun hasRepeatedNumberTest() {
        assertTrue(hasRepeatedNumber("111111"))
        assertTrue(hasRepeatedNumber("123356"))
        assertFalse(hasRepeatedNumber("123456"))
    }

    @Test
    fun hasNoDecreasingNumbersTest() {
        assertTrue(isOnlyIncreasing("111111"))
        assertTrue(isOnlyIncreasing("111123"))
        assertTrue(isOnlyIncreasing("135679"))
        assertFalse(isOnlyIncreasing("123465"))
    }
}
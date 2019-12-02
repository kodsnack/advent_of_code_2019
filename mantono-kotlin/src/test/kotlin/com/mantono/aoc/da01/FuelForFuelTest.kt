package com.mantono.aoc.da01

import com.mantono.aoc.fuelForFuel
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class FuelForFuelTest {
    @Test

    fun testFuelForFuel() {
        val fuelCost: Int = fuelForFuel(100756)
        assertEquals(50346, fuelCost)
    }
}
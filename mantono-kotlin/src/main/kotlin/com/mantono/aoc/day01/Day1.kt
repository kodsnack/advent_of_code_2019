package com.mantono.aoc.day01

import com.mantono.aoc.AoC
import com.mantono.aoc.Part

@AoC(1, Part.A)
fun a(input: String): Int {
    return input
        .split("\n")
        .map { it.toInt() }
        .map(::fuelCost)
        .sum()
}

@AoC(1, Part.B)
fun b(input: String): Int {
    return input
        .split("\n")
        .map { it.toInt() }
        .map(::fuelCost)
        .map { it + fuelForFuel(it) }
        .sum()
}

fun fuelCost(mass: Int): Int = (mass/3) - 2

tailrec fun fuelForFuel(mass: Int, totalMass: Int = 0): Int {
    val currentMass: Int = fuelCost(mass)
    return if(currentMass > 0) {
        fuelForFuel(currentMass, totalMass + currentMass)
    } else {
        totalMass
    }
}
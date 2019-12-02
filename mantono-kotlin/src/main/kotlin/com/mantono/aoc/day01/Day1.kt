package com.mantono.aoc.day01

fun a(input: String): Int {
    return input
        .split("\n")
        .map { it.toInt() }
        .map(::fuelCost)
        .sum()
}

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
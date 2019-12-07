package com.mantono.aoc.day06

import com.mantono.aoc.AoC
import com.mantono.aoc.Part

@AoC(6, Part.A)
fun calculateOrbits(input: String): Int {
    val orbits: Map<String, List<String>> = input.split("\n").asSequence()
        .map { it.trim().split(")") }
        .groupBy({ it.first() }, { it.last() })

    return computeRecursiveOrbits(orbits)
}

private fun computeRecursiveOrbits(
    orbits: Map<String, List<String>>,
    planet: String = "COM",
    depth: Int = 0
): Int {
    val orbitalChildren: List<String> = orbits.getOrDefault(planet, emptyList())
    val indent: String = String(Array<Char>(depth) { ' ' }.toCharArray())
    return if(orbitalChildren.isEmpty()) {
        println("$indent$planet ==> $depth")
        depth
    } else {
        println("$indent$planet ==> $orbitalChildren $depth")
        orbitalChildren.asSequence()
            .map { computeRecursiveOrbits(orbits, it, depth + 1) }
            .sum() + depth
    }
}
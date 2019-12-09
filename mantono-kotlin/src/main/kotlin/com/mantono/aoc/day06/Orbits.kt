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

@AoC(6, Part.B)
fun orbitalTransfers(input: String): Int {
    val orbits: Map<String, List<String>> = input.split("\n").asSequence()
        .map { it.trim().split(")") }
        .groupBy({ it.first() }, { it.last() })

    val pathToSanta: List<String> = findPathToObject(orbits, "SAN")
    val pathToMe: List<String> = findPathToObject(orbits, "YOU")
    val commonPath: Set<String> = pathToSanta.toSet().intersect(pathToMe.toSet())
    return (pathToMe.size + pathToSanta.size) - (commonPath.size * 2) - 2
}

fun findPathToObject(
    orbits: Map<String, List<String>>,
    destination: String,
    path: List<String> = emptyList(),
    current: String = "COM"
): List<String> {
    val updatedPath: List<String> = path + current
    if(destination == current) {
        return updatedPath
    }
    val options: List<String> = orbits.getOrDefault(current, emptyList())
    if(options.isEmpty()) {
        return emptyList()
    }
    return options.asSequence()
        .map { findPathToObject(orbits, destination, updatedPath, it) }
        .firstOrNull { it.contains(destination) } ?: emptyList()
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
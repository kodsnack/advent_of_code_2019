package com.mantono.aoc.day04

import com.mantono.aoc.AoC
import com.mantono.aoc.Part

@AoC(4, Part.A)
fun computePasswords1(input: String): Int {
    return createSequence(input)
        .filter(::isOnlyIncreasing)
        .filter(::hasRepeatedNumber)
        .filter { it.length == 6 }
        .count()
}

@AoC(4, Part.B)
fun computePasswords2(input: String): Int {
    return createSequence(input)
        .filter(::isOnlyIncreasing)
        .filter(::hasOnlyRepeatedTwice)
        .filter { it.length == 6 }
        .count()
}

fun createSequence(input: String): Sequence<String> {
    val inputs: List<String> = input.trim().split("-")
    val from: Int = inputs[0].toInt()
    val to: Int = inputs[1].toInt()
    return (from until to).asSequence().map { it.toString() }
}

fun hasRepeatedNumber(n: String): Boolean = n
    .zipWithNext() { a, b -> a == b }
    .any { it }

fun isOnlyIncreasing(n: String): Boolean = n
    .zipWithNext { a, b -> a <= b }
    .all { it }

fun hasOnlyRepeatedTwice(n: String): Boolean {
    val occurrences: Map<Int, List<Int>> = n.asSequence()
        .map { it.toString().toInt() }
        .mapIndexed { index, number -> number to index }
        .groupBy({ it.first }, { it.second })

    return occurrences
        .filter { it.value.size == 2 }
        .filter { it.value[0] + 1 == it.value[1] }
        .any()
}
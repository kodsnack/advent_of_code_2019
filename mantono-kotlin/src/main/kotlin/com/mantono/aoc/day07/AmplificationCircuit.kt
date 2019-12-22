package com.mantono.aoc.day07

import com.mantono.aoc.AoC
import com.mantono.aoc.IntCodeComputer
import com.mantono.aoc.Part
import java.util.*

@AoC(7, Part.A)
fun amplificationCircuit(input: String): Int {
    return phaseSettingsGenerator(listOf(0, 1, 2, 3, 4))
        .map { setting ->
            val computer = IntCodeComputer.loadProgram(input)
            setting.asSequence().drop(1).fold(computer.run(dequeOf(setting.first, 0)).pop()) { output: Int, phaseSetting: Int ->
                computer.reboot()
                val result: Deque<Int> = computer.run(dequeOf<Int>(phaseSetting, output))
                result.pop()
            }
        }
        .sortedDescending()
        .onEach(::println)
        .first()
}

//@AoC(7, Part.B)
//fun amplificationCircuitWithFeedBackLoop(input: String): Int {
//    return phaseSettingsGenerator(listOf(5, 6, 7, 8, 9))
//        .map { setting ->
//            setting.asSequence().drop(1).fold(runProgram(input, dequeOf(setting.first, 0))) { acc: Int, i: Int ->
//                runProgram(input, dequeOf<Int>(i, acc))
//            }
//        }
//        .sortedDescending()
//        .onEach(::println)
//        .first()
//}

fun <T> dequeOf(vararg i: T): Deque<T> {
    val list = LinkedList<T>()
    list.addAll(i)
    return list
}

fun phaseSettingsGenerator(configuration: List<Int>): Sequence<Deque<Int>> {
    val from: Int = configuration.sorted().joinToString(separator = "") { it.toString() }.toInt()
    val to: Int = configuration.sortedDescending().joinToString(separator = "") { it.toString() }.toInt()
    val range = IntProgression.fromClosedRange(from, to, 9)
    return range.asSequence()
        .filter { onTarget(it, configuration) }
        .map { it.toString().padStart(5, '0') }
        .map { number -> number.map { digit -> digit.toString().toInt() } }
        .map { LinkedList(it) }
}

private fun onTarget(i: Int, configuration: List<Int>): Boolean {
    val digits: List<Int> = i.toString().padStart(5, '0').map { it.toString().toInt() }.sorted()
    return digits == configuration.sorted()
}

//fun factorial(i: Int): Int = if(i == 1) i * 1 else i * factorial(i - 1)
//
//private fun List<Int>.next(): List<Int>? {
//    if(this == listOf(4, 3, 2, 1, 0)) {
//        return null
//    }
//    val nextList: MutableList<Int> = ArrayList(this)
//    val (i0, i1) = this.indices.reversed()
//        .asSequence()
//        .zipWithNext { i0, i1 -> Pair(i0, i1) }
//        .first { (i0, i1) -> this[i0] > this[i1] }
//
//    val value0 = this[i0]
//    val value1 = this[i1]
//    nextList[i0] = value1
//    nextList[i1] = value0
//    return nextList
//}
package com.mantono.aoc.day07

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import com.mantono.aoc.runProgram
import java.util.*

@AoC(7, Part.A)
fun amplificationCircuit(input: String): Int {
    return phaseSettingsGenerator()
        .map { setting ->
            setting.asSequence().drop(1).fold(runProgram(input, dequeOf(setting.first, 0))) { acc: Int, i: Int ->
                runProgram(input, dequeOf<Int>(i, acc))
            }
        }
        .sortedDescending()
        .onEach(::println)
        .first()
}

//@AoC(5, Part.B)
fun intCodeExtended(input: String): Int {
    return runProgram(input, 5)
}

fun <T> dequeOf(vararg i: T): Deque<T> {
    val list = LinkedList<T>()
    list.addAll(i)
    return list
}

fun phaseSettingsGenerator(): Sequence<Deque<Int>> {
    val range = IntProgression.fromClosedRange(1234, 43210, 9)
    return range.asSequence()
        .filter(::onTarget)
        .map { it.toString().padStart(5, '0') }
        .map { number -> number.map { digit -> digit.toString().toInt() } }
        .map { LinkedList(it) }
}

private fun onTarget(i: Int): Boolean {
    val string = i.toString().padStart(5, '0')
    return string.run {
        contains("0") && contains("1") && contains("2") && contains("3") && contains("4")
    }
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
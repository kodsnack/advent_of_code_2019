package com.mantono.aoc.day07

import com.mantono.aoc.AoC
import com.mantono.aoc.IntCodeComputer
import com.mantono.aoc.Part
import com.mantono.aoc.State
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
        .first()
}

@AoC(7, Part.B)
fun amplificationCircuitWithFeedBackLoop(input: String): Int {
    return phaseSettingsGenerator(listOf(5, 6, 7, 8, 9))
        .map { runWithFeedbackLoop(it, input) }
        .sortedDescending()
        .first()
}

fun runWithFeedbackLoop(settings: Deque<Int>, program: String): Int {
    val computers: Array<IntCodeComputer> = Array(5) { IntCodeComputer.loadProgram(program) }
    var signal: Int = 0
    var i: Int = 0
    while(computers.last().state() != State.Halted) {
        signal = if(settings.isNotEmpty()) {
            computers[i++ % 5].run(dequeOf(settings.pop(), signal)).pop()
        } else {
            computers[i++ % 5].run(signal).pop()
        }
    }
    return signal
}

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
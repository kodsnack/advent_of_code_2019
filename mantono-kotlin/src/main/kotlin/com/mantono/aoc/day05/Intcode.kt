package com.mantono.aoc.day05

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import java.util.*
import kotlin.collections.ArrayList
import kotlin.math.log10
import kotlin.math.pow

typealias Memory = MutableList<Int>
typealias Execution = (memory: Memory, data: List<Int>, address: Int, output: Deque<Int>) -> Unit

enum class OpCode(
    val code: Int,
    val inputs: Int,
    val execute: Execution
) {
    ADD(1 , 3, { mem, data, _, _ ->
        val modes: List<Mode> = Mode.parse(data[0])
        val i0: Int = mem.read(modes[0], data[1])
        val i1: Int = mem.read(modes[1], data[2])
        val resultAddr: Int = data[3]
        mem[resultAddr] = i0 + i1
    }),
    MULT(2, 3, { mem, data, _, _ ->
        val modes: List<Mode> = Mode.parse(data[0])
        val i0: Int = mem.read(modes[0], data[1])
        val i1: Int = mem.read(modes[1], data[2])
        mem[data[3]] = i0 + i1
    }),
    INPUT(3, 1, { mem, data, _, _ ->
        val i = data[1]
        mem[i] = 1
    }),
    OUTPUT(4, 2, { mem, data, _, output ->
        val modes: List<Mode> = Mode.parse(data[0])
        val i0: Int = mem.read(modes[0], data[1])
        output.push(i0)
    }),
    HALT(99, 0, { mem, _, _, _ ->
        mem[0]
    });

    companion object {
        fun parse(instruction: Int): OpCode {
            val opCode: Int = instruction % 10
            return values()
                .firstOrNull { it.code == opCode } ?: error("Unsupported opcode: '$instruction'")
        }
    }
}

enum class Mode {
    Address,
    Value;

    companion object {
        fun parse(opCode: Int): List<Mode> {
            return (1..4).asSequence()
                .map { 10.0.pow(it.toDouble()).toInt() }
                .map { opCode / it }
                .map { values()[it % 2] }
                .toList()
        }
    }
}

fun Memory.read(mode: Mode, address: Int): Int {
    return when(mode) {
        Mode.Address -> this[address]
        Mode.Value -> address
    }
}

@AoC(5, Part.A)
fun intCode(input: String): Int {
    val memory: MutableList<Int> = input.split(",")
        .map { it.trim().toInt() }
        .toMutableList()

    return parseData(memory)
}

tailrec fun parseData(memory: MutableList<Int>, index: Int = 0, output: Deque<Int> = LinkedList()): Int {
    Thread.sleep(200L)
    val opCode = OpCode.parse(memory[index])
    val from: Int = index
    val to: Int = from + opCode.inputs
    val instructions: List<Int> = memory[from..to]
    val line: String = memory.mapIndexed { i, n ->
        when {
            i == from -> "[$n"
            i == to -> "$n]"
            else -> n.toString()
        }
    }.joinToString(separator = ", ") { it.toString() }
    println(line)
    opCode.execute(memory, instructions, index, output)
    val address: Int = to + 1
    if(opCode == OpCode.HALT) {
        println(output.joinToString { it.toString() })
        return memory[address]
    }
    return parseData(memory, address, output)
}

operator fun <T> List<T>.get(range: IntRange): List<T> {
    return subList(range.first, range.last+1)
}
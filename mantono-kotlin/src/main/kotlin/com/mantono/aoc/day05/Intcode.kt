package com.mantono.aoc.day05

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import java.util.*
import kotlin.collections.ArrayList
import kotlin.math.log10
import kotlin.math.pow

typealias Memory = MutableList<Int>
typealias Instruction = Deque<Int>
typealias Execution = (memory: Memory, input: Instruction, address: Int) -> Unit

enum class OpCode(
    val code: Int,
    val inputs: Int,
    val execute: Execution
) {
    ADD(1 , 3, { mem, input, _ ->
        val modes: List<Mode> = Mode.parse(input.pop())
        val i0: Int = mem.read(modes[0], input.pop())
        val i1: Int = mem.read(modes[1], input.pop())
        mem[input.pop()] = i0 + i1
    }),
    MULT(2, 3, { mem, input, _ ->
        val modes: kotlin.collections.List<com.mantono.aoc.day05.Mode> = com.mantono.aoc.day05.Mode.parse(input.pop())
        val i0: kotlin.Int = mem.read(modes[0], input.pop())
        val i1: kotlin.Int = mem.read(modes[1], input.pop())
        mem[input.pop()] = i0 + i1
    }),
    STORE(3, 1, { mem, input, addr ->
        // TODO FIX Me!
        mem[mem[addr]] = mem[addr+1]
    }),
    READ(4, 2, { mem, input, addr ->
        // TODO FIX Me!
        mem[addr+2]
    }),
    HALT(99, 0, { mem, _, _ ->
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
    input.split(",").map { it.trim().toInt() }
        .forEach { if(it in 1..4) print("\n$it, ") else print("$it, ") }
    val memory: MutableList<Int> = input.split(",")
        .map { it.trim().toInt() }
        .toMutableList()
    return parseData(memory)
}

tailrec fun parseData(memory: MutableList<Int>, index: Int = 0): Int {
    val opCode = OpCode.parse(memory[index])
    val from: Int = index
    val to: Int = from + opCode.inputs
    val instructions: Instruction = LinkedList(memory[from..to])
    //val input: List<Int> = readDataInMemory(memory, modes, index, opCode.inputs)
    opCode.execute(memory, instructions, index)
    val address: Int = to + 1
    if(opCode == OpCode.HALT) {
        return memory[address]
    }
    return parseData(memory, address)
}

operator fun <T> List<T>.get(range: IntRange): List<T> {
    return subList(range.first, range.last+1)
}
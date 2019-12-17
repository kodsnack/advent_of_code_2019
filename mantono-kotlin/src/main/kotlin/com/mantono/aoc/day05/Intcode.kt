package com.mantono.aoc.day05

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import java.util.*
import kotlin.math.pow

typealias Memory = MutableList<Int>
typealias Execution = (memory: Memory, addr: Int, output: Deque<Int>) -> Int

enum class OpCode(
    val code: Int,
    val execute: Execution
) {
    ADD(1 , { mem, addr, _ ->
        val modes: List<Mode> = Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], mem[addr+1])
        val i1: Int = mem.read(modes[1], mem[addr+2])
        mem[mem[addr+3]] = i0 + i1
        addr + 4
    }),
    MULT(2, { mem, addr, _ ->
        val modes: List<Mode> = Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], mem[addr+1])
        val i1: Int = mem.read(modes[1], mem[addr+2])
        mem[mem[addr+3]] = i0 * i1
        addr + 4
    }),
    INPUT(3, { mem, addr, _ ->
        val i = mem[addr+1]
        mem[i] = 1
        addr+2
    }),
    OUTPUT(4, { mem, addr, output ->
        val modes: List<Mode> = Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], mem[addr+1])
        println(i0)
        output.push(i0)
        addr+2
    }),
    HALT(99, { mem, _, _ ->
        mem[0]
    });

    companion object {
        fun parse(instruction: Int): OpCode {
            val opCode: Int = instruction % 100
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
            return (2..4).asSequence()
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

tailrec fun parseData(memory: MutableList<Int>, pointer: Int = 0, output: Deque<Int> = LinkedList()): Int {
    Thread.sleep(200L)
    val opCode = OpCode.parse(memory[pointer])
    val pointerMoved: Int = opCode.execute(memory, pointer, output)
    dumpMemory(memory, pointer, pointerMoved)
    if(opCode == OpCode.HALT) {
        println(output.joinToString { it.toString() })
        return memory[pointerMoved]
    }
    return parseData(memory, pointerMoved, output)
}

fun dumpMemory(memory: Memory, from: Int, to: Int) {
    val line: String = memory.mapIndexed { i, n ->
        when(i) {
            from -> "[$n"
            to - 1 -> "$n]"
            else -> n.toString()
        }
    }.joinToString(separator = ", ") { it }
    println(line)
}

operator fun <T> List<T>.get(range: IntRange): List<T> {
    return subList(range.first, range.last+1)
}
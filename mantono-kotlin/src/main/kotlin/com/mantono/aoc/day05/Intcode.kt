package com.mantono.aoc.day05

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import java.util.*
import kotlin.math.pow

typealias Memory = MutableList<Int>
typealias Execution = (memory: Memory, addr: Int, input: Deque<Int>, output: Deque<Int>) -> Int

enum class OpCode(
    val code: Int,
    val execute: Execution
) {
    ADD(1 , { mem, addr, _, _ ->
        val modes: List<Mode> = Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], addr+1)
        val i1: Int = mem.read(modes[1], addr+2)
        mem[mem[addr+3]] = i0 + i1
        addr + 4
    }),
    MULT(2, { mem, addr, _, _ ->
        val modes: List<Mode> = Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], addr+1)
        val i1: Int = mem.read(modes[1], addr+2)
        mem[mem[addr+3]] = i0 * i1
        addr + 4
    }),
    INPUT(3, { mem, addr, input, _ ->
        val i = mem[addr+1]
        mem[i] = input.poll()
        addr+2
    }),
    OUTPUT(4, { mem, addr, _, output ->
        val modes: List<Mode> = Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], addr+1)
        println(i0)
        output.push(i0)
        addr+2
    }),
    JUMP_IF_TRUE(5, { mem, addr, _, _ ->
        if(mem[addr+1] != 0) {
            val modes: List<Mode> = Mode.parse(mem[addr])
            mem.read(modes[0], addr+2)
        } else {
            addr+3
        }
    }),
    JUMP_IF_FALSE(6, { mem, addr, _, _ ->
        if(mem[addr+1] == 0) {
            val modes: List<Mode> = Mode.parse(mem[addr])
            mem.read(modes[0], addr+2)
        } else {
            addr+3
        }
    }),
    LESS_THAN(7, { mem, addr, _, _ ->
        TODO("Opcode 7 not done yet")
    }),
    EQUALS(8, { mem, addr, _, _ ->
        TODO("Opcode 8 not done yet")
    }),
    HALT(99, { mem, _, _, _ ->
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
        Mode.Address -> this[this[address]]
        Mode.Value -> this[address]
    }
}

@AoC(5, Part.A)
fun intCode(input: String): Int {
    return parseMemory(input, 1)
}

@AoC(5, Part.B)
fun intCodeExtended(input: String): Int {
    return parseMemory(input, 5)
}

fun parseMemory(program: String, input: Int): Int {
    val memory: MutableList<Int> = program.split(",")
        .map { it.trim().toInt() }
        .toMutableList()

    return parseData(memory, input = LinkedList(listOf(input)))
}

tailrec fun parseData(
    memory: MutableList<Int>,
    pointer: Int = 0,
    input: Deque<Int>,
    output: Deque<Int> = LinkedList()
): Int {
    Thread.sleep(200L)
    dumpMemory(memory, pointer)
    val opCode = OpCode.parse(memory[pointer])
    val pointerMoved: Int = opCode.execute(memory, pointer, input, output)
    check(pointerMoved in memory.indices) {
        "Address is outside memory: $pointerMoved"
    }
    if(opCode == OpCode.HALT) {
        println(output.joinToString { it.toString() })
        return memory[pointerMoved]
    }
    return parseData(memory, pointerMoved, input, output)
}

fun dumpMemory(memory: Memory, pointer: Int) {
    val line: String = memory.mapIndexed { i, n ->
        when(i) {
            pointer -> "[$n]"
            else -> n.toString()
        }
    }.joinToString(separator = ", ") { it }
    println(line)
}

operator fun <T> List<T>.get(range: IntRange): List<T> {
    return subList(range.first, range.last+1)
}
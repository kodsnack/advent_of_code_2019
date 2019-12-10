package com.mantono.aoc.day05

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import java.util.*
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
        mem[input[2]] = input[0] + input[1]
    }),
    MULT(2, 3, { mem, input, _ ->
        mem[input[2]] = input[0] * input[1]
    }),
    STORE(3, 1, { mem, input, addr ->
        mem[input[0]] = input[0]
        mem[mem[addr]] = mem[addr+1]
    }),
    READ(4, 2, { mem, input, addr ->
        mem[addr+2]
    }),
    HALT(99, 0, { mem, _, _ ->
        mem[0]
    });

    companion object {
        fun parse(instruction: Int): Pair<OpCode, List<Mode>> {
            val a: Mode = Mode.values()[instruction.getDigitFromRight(4)]
            val b: Mode = Mode.values()[instruction.getDigitFromRight(3)]
            val c: Mode = Mode.values()[instruction.getDigitFromRight(2)]
            val opcode: Int = instruction % 10
            val actualCode = OpCode.values()
                .firstOrNull { it.code == opcode } ?: error("Unsupported opcode: '$instruction'")
            return actualCode to listOf(a, b, c)
        }
    }
}

private fun readDataInMemory(memory: Memory, modes: List<Mode>, address: Int, sizeOfInputs: Int): List<Int> {
    return modes.asSequence()
        .take(sizeOfInputs)
        .mapIndexed { index: Int, mode: Mode ->
            val input: Int = memory[address + index + 1]
            when(mode) {
                Mode.Address -> memory[input]
                Mode.Value -> input
            }
        }
        .toList()
}

fun Int.getDigitFromRight(i: Int): Int {
    val divideBy: Int = 10.0.pow(i.toDouble()).toInt()
    return (this / divideBy) % 2
}

enum class Mode {
    Address,
    Value
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
    val (opCode, modes) = OpCode.parse(memory[index])
    //val input: List<Int> = readDataInMemory(memory, modes, index, opCode.inputs)
    opCode.execute(memory, input, index)
    val address: Int = index + input.size + 1
    if(opCode == OpCode.HALT) {
        return memory[address]
    }
    return parseData(memory, address)
}
package com.mantono.aoc.day05

import kotlin.math.pow
import kotlin.math.sqrt

typealias Instruction = (memory: MutableList<Int>, address: Int) -> Int

enum class OpCode(
    val code: Int,
    val execute: Instruction
) {
    ADD(1 ,{ mem, addr ->
        val input0: Int = mem[addr+1]
        val input1: Int = mem[addr+2]
        val outputAddress: Int = mem[addr+3]
        mem[outputAddress] = input0 + input1
        outputAddress
    }),
    MULT(2, { mem, addr ->
        val input0: Int = mem[addr+1]
        val input1: Int = mem[addr+2]
        val outputAddress: Int = mem[addr+3]
        mem[outputAddress] = input0 * input1
        outputAddress
    }),
    STORE(3, { mem, addr ->
        mem[addr] = addr
        addr + 1
    }),
    READ(4, { mem, addr ->
        mem[addr]
    }),
    HALT(99, { mem, _ ->
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

fun Int.getDigitFromRight(i: Int): Int {
    val divideBy: Int = 10.0.pow(i.toDouble()).toInt()
    return (this / divideBy) % 2
}

enum class Mode {
    Position,
    Immediate
}


tailrec fun parseData(inputData: MutableList<Int>, index: Int = 0): Int {
    val (opCode, modes) = OpCode.parse(inputData[index])
    val address: Int = opCode.execute(inputData, index)
    if(opCode == OpCode.HALT) {
        return address
    }
    return parseData(inputData, address)
}
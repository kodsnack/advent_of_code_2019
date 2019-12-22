package com.mantono.aoc

import com.mantono.aoc.day07.dequeOf
import java.util.*
import kotlin.collections.ArrayList
import kotlin.math.pow

typealias Execution = (memory: Memory, addr: Int, input: Deque<Int>, output: Deque<Int>) -> Int

class IntCodeComputer(
    private var memory: Memory,
    private var pointer: Int = 0
) {
    private val initialMemoryState: ReadOnlyMemory = ArrayList(memory)

    fun reboot(): IntCodeComputer {
        this.memory = ArrayList(initialMemoryState)
        this.pointer = 0
        return this
    }

    fun run(input: Deque<Int>): Deque<Int> {
        return executeProgram(input)
    }

    fun run(input: Int): Deque<Int> {
        return executeProgram(dequeOf(input))
    }

    private tailrec fun executeProgram(
        input: Deque<Int>,
        output: Deque<Int> = LinkedList()
    ): Deque<Int> {
        dumpMemory(memory, pointer)
        val opCode = OpCode.parse(memory[pointer])
        pointer = opCode.execute(memory, pointer, input, output)
        check(pointer in memory.indices) {
            "Address is outside memory: $pointer"
        }
        if(opCode == OpCode.HALT) {
            println(output.joinToString { it.toString() })
            return output
        }
        return executeProgram(input, output)
    }

    private fun dumpMemory(memory: Memory, pointer: Int) {
        val line: String = memory.mapIndexed { i, n ->
            when(i) {
                pointer -> "[$n]"
                else -> n.toString()
            }
        }.joinToString(separator = ", ") { it }
        println(line)
    }

    operator fun get(address: Int): Int = memory[address]

    companion object {
        fun loadProgram(program: String): IntCodeComputer {
            val memory: MutableList<Int> = program.split(",")
                .map { it.trim().toInt() }
                .toMutableList()

            return IntCodeComputer(memory)
        }
    }
}

enum class OpCode(
    val code: Int,
    val execute: Execution
) {
    ADD(1 , { mem, addr, _, _ ->
        val modes: List<Mode> =
            Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], addr+1)
        val i1: Int = mem.read(modes[1], addr+2)
        mem.write(addr+3, i0 + i1)
        addr + 4
    }),
    MULT(2, { mem, addr, _, _ ->
        val modes: List<Mode> =
            Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], addr+1)
        val i1: Int = mem.read(modes[1], addr+2)
        mem.write(addr+3, i0 * i1)
        addr + 4
    }),
    INPUT(3, { mem, addr, input, _ ->
        val i = mem[addr+1]
        mem[i] = input.poll()
        addr+2
    }),
    OUTPUT(4, { mem, addr, _, output ->
        val modes: List<Mode> =
            Mode.parse(mem[addr])
        val i0: Int = mem.read(modes[0], addr+1)
        println(i0)
        output.push(i0)
        addr+2
    }),
    JUMP_IF_TRUE(5, { mem, addr, _, _ ->
        val modes: List<Mode> =
            Mode.parse(mem[addr])
        if(mem.read(modes[0], addr+1) != 0) {
            mem.read(modes[1], addr+2)
        } else {
            addr+3
        }
    }),
    JUMP_IF_FALSE(6, { mem, addr, _, _ ->
        val modes: List<Mode> =
            Mode.parse(mem[addr])
        if(mem.read(modes[0], addr+1) == 0) {
            mem.read(modes[1], addr+2)
        } else {
            addr+3
        }
    }),
    LESS_THAN(7, { mem, addr, _, _ ->
        val modes: List<Mode> =
            Mode.parse(mem[addr])
        val first: Int = mem.read(modes[0], addr+1)
        val second: Int = mem.read(modes[1], addr+2)
        mem.write(addr+3, if(first < second) 1 else 0)
        addr+4
    }),
    EQUALS(8, { mem, addr, _, _ ->
        val modes: List<Mode> =
            Mode.parse(mem[addr])
        val first: Int = mem.read(modes[0], addr+1)
        val second: Int = mem.read(modes[1], addr+2)
        mem.write(addr+3, if(first == second) 1 else 0)
        addr+4
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
    /**
     * Position mode
     */
    Address,
    /**
     * Immediate mode
     */
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
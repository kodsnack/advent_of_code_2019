package com.mantono.aoc.day02

import com.mantono.aoc.AoC
import com.mantono.aoc.Part

@AoC(2, Part.A)
fun a(input: String): Int {
    return runProgram(input, 12, 2)
}

@AoC(2, Part.B)
fun b(input: String): Int {
    for(i in 0..99) {
        for(n in 0..99) {
            val result: Int = runProgram(input, i, n)
            if(result == 19690720) {
                return (100 * i) + n
            }
        }
    }
    return -1
}

fun runProgram(input: String, programState0: Int, programState1: Int): Int {
    val inputData: MutableList<Int> = input.split(",").map { it.trim().toInt() }.toMutableList()
    inputData[1] = programState0
    inputData[2] = programState1
    return parseData(inputData)
}

tailrec fun parseData(inputData: MutableList<Int>, index: Int = 0): Int {
    val opCode: Int = inputData[index]
    val input0: Int = inputData[index+1]
    val input1: Int = inputData[index+2]
    val output: Int = inputData[index+3]
    when(opCode) {
        1 -> inputData[output] = inputData[input0] + inputData[input1]
        2 -> inputData[output] = inputData[input0] * inputData[input1]
        99 -> return inputData[0]
        else -> error("Unexpected op code: $opCode")
    }
    return parseData(inputData, index + 4)
}

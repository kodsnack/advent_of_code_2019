package com.mantono.aoc.day02

fun a(input: String): Int {
    val inputData: MutableList<Int> = input.split(",").map { it.trim().toInt() }.toMutableList()
    inputData[1] = 12
    inputData[2] = 2
    return parseData(inputData)
}

fun b(input: String): Int {
    for(i in 0..99) {
        for(n in 0..99) {
            val inputData: MutableList<Int> = input.split(",").map { it.trim().toInt() }.toMutableList()
            inputData[1] = i
            inputData[2] = n
            val result: Int = parseData(inputData)
            if(result == 19690720) {
                println("100 * $i + $n")
                return (100 * i) + n
            }
        }
    }
    return -1
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

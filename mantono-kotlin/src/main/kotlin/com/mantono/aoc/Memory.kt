package com.mantono.aoc

typealias Memory = MutableList<Int>
typealias ReadOnlyMemory = List<Int>

fun Memory.read(mode: Mode, address: Int): Int {
    return when(mode) {
        Mode.Address -> this[this[address]]
        Mode.Value -> this[address]
    }
}

fun Memory.write(address: Int, value: Int) {
    require(address in this.indices) {
        "Memory address '$address' does not exist"
    }
    this[this[address]] = value
}

operator fun Memory.get(range: IntRange): Memory {
    return subList(range.first, range.last+1)
}
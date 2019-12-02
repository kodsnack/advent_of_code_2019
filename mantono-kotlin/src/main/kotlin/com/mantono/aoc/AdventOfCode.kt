package com.mantono.aoc

@Retention(AnnotationRetention.RUNTIME)
annotation class AoC(val day: Int, val part: Part)

enum class Part {
    A,
    B
}
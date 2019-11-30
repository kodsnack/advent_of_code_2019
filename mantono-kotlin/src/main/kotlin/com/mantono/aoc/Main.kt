package com.mantono.aoc

import java.io.File
import java.nio.file.Files

fun main() {
	val file = File("input")
	Files.readAllLines(file.toPath()).asSequence()
}
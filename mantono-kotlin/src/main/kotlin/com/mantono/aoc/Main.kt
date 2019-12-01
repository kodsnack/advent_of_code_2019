package com.mantono.aoc

import com.mantono.aoc.day01.b
import java.io.File
import java.io.FileOutputStream
import java.net.URL
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel
import java.nio.file.Files
import java.time.LocalDateTime

fun main(args: Array<String>) {
    val day: Int = day(args)
    val file = downloadInput(day)
    val input = Files.readAllLines(file.toPath()).asSequence()
    // Change between a / b here
    val result: Int = b(input)
    println(result)
}

fun day(args: Array<String>): Int {
    val argsDay: Int? = args.asSequence().firstOrNull()?.toInt()
    val today: Int = LocalDateTime.now().dayOfMonth
    return argsDay ?: today
}

private const val INPUTS_URL: String = "https://adventofcode.com/2019/day"

/**
 * Download the inputs for the current day's assignment, unless they are already
 * present on disk
 */
fun downloadInput(day: Int): File {
    val target = File("src/resources/inputs/$day")
    if(!target.exists()) {
        val fullUrl = URL("$INPUTS_URL/$day/input")
        val channel: ReadableByteChannel = Channels.newChannel(fullUrl.openStream())
        val stream = FileOutputStream(target)
        stream.channel.transferFrom(channel, 0, Long.MAX_VALUE)
    }
    return target
}
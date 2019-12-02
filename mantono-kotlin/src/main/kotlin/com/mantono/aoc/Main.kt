package com.mantono.aoc

import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.File
import java.net.URL
import java.nio.file.Files
import java.time.LocalDateTime

fun main(args: Array<String>) {
    val day: Int = day(args)
    println("Use inputs for problem for day $day")
    val file: File = inputFile(day)
    val input = String(Files.readAllBytes(file.toPath()))
    val result = findFunction(day).invoke(null, input)
    println(result)
}

fun day(args: Array<String>): Int {
    val argsDay: Int? = args.asSequence().firstOrNull()?.toInt()
    val today: Int = LocalDateTime.now().dayOfMonth
    return argsDay ?: today
}

private const val INPUTS_STORAGE: String = "src/main/resources/inputs"
private const val INPUTS_URL: String = "https://adventofcode.com/2019/day"

/**
 * Load the file containing the day's input data, and download it first if
 * required
 */
fun inputFile(day: Int): File {
    val target = File("$INPUTS_STORAGE/$day")
    if(!target.exists()) {
        downloadInput(day, target)
    }
    return target
}

/**
 * Download the inputs for the day's assignment
 */
private fun downloadInput(day: Int, target: File) {
    val fullUrl = URL("$INPUTS_URL/$day/input")
    val client = OkHttpClient()
    val session: String = System.getenv().getValue("AOC_SESSION")
    val request = Request.Builder()
        .url(fullUrl)
        .addHeader("Cookie", "session=$session")
        .build()
    val response = client.newCall(request).execute()
    val content: String = response.body!!.string()
    with(target) {
        writeText(content)
    }
}
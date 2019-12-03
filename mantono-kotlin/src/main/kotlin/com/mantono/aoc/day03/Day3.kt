package com.mantono.aoc.day03

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import java.lang.Integer.max
import java.lang.Integer.min
import kotlin.math.abs

private val startPosition = Position(0, 0)

@AoC(3, Part.A)
fun findIntersections(input: String): Int {
    val inputs: List<String> = input.split("\n")
    val firstCable: List<Path> = pathDirectionsToPath(inputs[0])
    val secondCable: List<Path> = pathDirectionsToPath(inputs[1])
    println(firstCable)
    println(secondCable)
    val positionsCable1: List<Position> = positionsFromPath(firstCable)
    val positionsCable2: List<Position> = positionsFromPath(secondCable)
    val overlaps: Set<Position> = positionsCable1.toSet().intersect(positionsCable2.toSet())
    println(overlaps)
    return overlaps
        .asSequence()
        .filterNot { it == startPosition }
        .map { it.manhattanDistance(startPosition) }
        .sorted()
        .first()
}

fun pathDirectionsToPath(pathDirections: String): List<Path> = pathDirections
    .split(",")
    .asSequence()
    .filterNot { it.isBlank() }
    .map { Path.parseFrom(it) }
    .toList()

data class Path(val direction: Direction, val distance: Int) {
    companion object {
        fun parseFrom(input: String): Path {
            val direction = Direction.fromString(input[0])
            val distance = input.drop(1).toInt()
            return Path(direction, distance)
        }
    }
}

data class Position(val x: Int, val y: Int) {
    fun walk(path: Path): Position {
        return when(path.direction) {
            Direction.Up -> this.copy(y = y + path.distance)
            Direction.Right -> this.copy(x = x + path.distance)
            Direction.Down -> this.copy(y = y - path.distance)
            Direction.Left -> this.copy(x = x - path.distance)
        }
    }

    fun manhattanDistance(other: Position): Int {
        val yDistance: Int = abs(this.y - other.y)
        val xDistance: Int = abs(this.x - other.x)
        return yDistance + xDistance
    }
}

fun positionsFromPath(fullPath: List<Path>): List<Position> {
    val visited: MutableList<Position> = ArrayList(fullPath.size*2)
    val finalDestination: Position = fullPath.asSequence()
        .fold(startPosition) { pos: Position, path: Path ->
            visited.add(pos)
            pos.walk(path)
        }
    visited.add(finalDestination)

    return visited.asSequence()
        .zipWithNext(::positionsBetween)
        .flatten()
        .distinct()
        .toList()
}


fun positionsBetween(pos0: Position, pos1: Position): List<Position> {
    require(pos0.x == pos1.x || pos0.y == pos1.y) {
        "Trying compare two positions that are not in a straight line to each other"
    }

    return if(pos0.x == pos1.x) {
        val line: IntRange = min(pos0.y, pos1.y) .. max(pos0.y, pos1.y)
        line.map { Position(pos0.x, it) }
    } else {
        val line: IntRange = min(pos0.x, pos1.x) .. max(pos0.x, pos1.x)
        line.map { Position(it, pos0.y) }
    }
}

enum class Direction {
    Up,
    Right,
    Down,
    Left;

    companion object {
        fun fromString(input: Char): Direction = values()
            .asSequence()
            .first { it.name.startsWith(input) }
    }
}
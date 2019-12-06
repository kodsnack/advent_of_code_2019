package com.mantono.aoc.day03

import com.mantono.aoc.AoC
import com.mantono.aoc.Part
import java.lang.Integer.max
import java.lang.Integer.min
import kotlin.math.abs

private val startPosition = Position(0, 0)
private typealias Path = List<Position>
private typealias MutablePath = MutableList<Position>

@AoC(3, Part.A)
fun findDistanceToClosestIntersection(input: String): Int {
    val (positionsCable0: Path, positionsCable1: Path) = positionsForPathFromInput(input)
    val overlaps: Set<Position> = positionsCable0.toSet().intersect(positionsCable1.toSet())

    return overlaps
        .asSequence()
        .filterNot { it == startPosition }
        .map { it.manhattanDistance(startPosition) }
        .sorted()
        .first()
}

@AoC(3, Part.B)
fun findDistanceToFirstIntersection(input: String): Int {
    val (positionsCable0: Path, positionsCable1: Path) = positionsForPathFromInput(input)
    val overlaps: Set<Position> = positionsCable0.toSet().intersect(positionsCable1.toSet())

    return overlaps.asSequence()
        .filterNot { it == startPosition }
        .map { intersection: Position ->
            val path0Intersection: Path = positionsCable0.takeWhile { it != intersection } + intersection
            val path1Intersection: Path = positionsCable1.takeWhile { it != intersection } + intersection
            val distance0: Int = distanceForPath(path0Intersection)
            val distance1: Int = distanceForPath(path1Intersection)
            distance0 + distance1
        }
        .sorted()
        .first()
}

fun positionsForPathFromInput(input: String): Pair<Path, Path> {
    val inputs: List<String> = input.split("\n")
    val firstCable: List<Vector> = pathDirectionsToPath(inputs[0])
    val secondCable: List<Vector> = pathDirectionsToPath(inputs[1])
    return positionsFromPath(firstCable) to positionsFromPath(secondCable)
}

fun distanceForPath(path: Path): Int = path
    .zipWithNext { p0: Position, p1: Position -> p0.manhattanDistance(p1) }
    .sum()

fun pathDirectionsToPath(pathDirections: String): List<Vector> = pathDirections
    .split(",")
    .asSequence()
    .filterNot { it.isBlank() }
    .map { Vector.parseFrom(it) }
    .toList()

data class Vector(val direction: Direction, val distance: Int) {
    companion object {
        fun parseFrom(input: String): Vector {
            val direction = Direction.fromString(input[0])
            val distance = input.drop(1).toInt()
            return Vector(direction, distance)
        }
    }
}

data class Position(val x: Int, val y: Int) {
    fun walk(path: Vector): Position {
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

fun positionsFromPath(fullPath: List<Vector>): Path {
    val visited: MutablePath = ArrayList(fullPath.size*2)
    val finalDestination: Position = fullPath.asSequence()
        .fold(startPosition) { pos: Position, path: Vector ->
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

fun positionsBetween(pos0: Position, pos1: Position): Path {
    require(pos0.x == pos1.x || pos0.y == pos1.y) {
        "Trying compare two positions that are not in a straight line to each other"
    }

     return if(pos0.x == pos1.x) {
         val line: IntProgression = if(pos0.y <= pos1.y) {
             min(pos0.y, pos1.y) .. max(pos0.y, pos1.y)
         } else {
             max(pos0.y, pos1.y) downTo min(pos0.y, pos1.y)
         }
         line.map { Position(pos0.x, it) }
    } else {
         val line: IntProgression = if(pos0.x <= pos1.x) {
             min(pos0.x, pos1.x) .. max(pos0.x, pos1.x)
         } else {
             max(pos0.x, pos1.x) downTo min(pos0.x, pos1.x)
         }
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
/* Advent of code 2019, day 1, part 1 in Kotlin 1.3 */
import java.io.File

fun main() {
	val fn = "day1.txt"
	val mass = File(fn)
		.readLines()
		.map(String::toInt)
		.map { it / 3 - 2 }
		.reduce(Int::plus)

	println(mass)
}
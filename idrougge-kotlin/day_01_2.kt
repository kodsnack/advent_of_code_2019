/* Advent of code 2019, day 1, part 2 in Kotlin 1.3 */
import java.io.File

fun fuel(mass: Int) : Int {
	val mass = (mass / 3 - 2).takeIf { it > 0 } ?: return 0
	return mass + fuel(mass)
}

fun main() {
	val fn = "day1.txt"
	val mass = File(fn)
		.readLines()
		.map(String::toInt)
		.map(::fuel)
		.reduce(Int::plus)

	println(mass)
}
/* Advent of code 2019, day 6, part 1 in ANSI REXX */
parse arg file
if file = '' then file = day6.txt

do while lines(file)
	parse value linein(file) with centre ')' satellite
	map.satellite = centre
	push satellite
end

total = 0

do while queued() > 0
	pull satellite
	total = total + track(satellite)
end

say total
exit

track: procedure expose map.
arg body
mother = map.body
if mother = 'COM' then return 1
else return track(mother) + 1

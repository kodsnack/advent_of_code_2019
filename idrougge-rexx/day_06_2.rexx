/* Advent of code 2019, day 6, part 2 in ANSI REXX */
parse arg file
if file = '' then file = day6.txt

do while lines(file)
	parse value linein(file) with centre ')' satellite
	map.satellite = centre
end

you = track(you)
san = track(san)
start = compare(you, san)
you = substr(you, start)
san = substr(san, start)
say words(you) - 1 + words(san) - 1
exit

track: procedure expose map.
arg body
mother = map.body
if mother = 'COM' then return ''
else return track(mother) body

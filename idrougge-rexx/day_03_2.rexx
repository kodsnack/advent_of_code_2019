/* Advent of code 2019, day 3, part 2 in ANSI REXX */
parse arg file
if file = '' then file = 'day3.txt'
min_steps = 999999999

do # = 0 while lines(file)
	line = linein(file)
	total_steps = 0
	call read
end
say min_steps
exit

read:
x = 0; y = 0
do while line > ''
	parse var line direction +1 steps ',' line
	dx = 0; dy = 0
	select
		when direction = 'U' then dy = -1
		when direction = 'D' then dy = +1
		when direction = 'L' then dx = -1
		when direction = 'R' then dx = +1
		otherwise signal bad_input
	end
	call plot #, x, y, dx, dy, steps, total_steps
	x = x + steps * dx
	y = y + steps * dy
	total_steps = total_steps + steps
end
return

plot: procedure expose map. closest min_steps
arg #, x, y, dx, dy, steps, total_steps
do i = 1 to steps
	x = x + dx
	y = y + dy
	if symbol(map.#.x.y.z) = lit then do
		map.#.x.y = '.'
		map.#.x.y.z = total_steps + i
	end
	if # & datatype(map.0.x.y.z) = num then do
		min_steps = min(min_steps, map.0.x.y.z + total_steps + i)
	end
end
return

bad_input:
say 'Bad input:' direction
exit 1
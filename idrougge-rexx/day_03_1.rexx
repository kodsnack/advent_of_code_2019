/* Advent of code 2019, day 3, part 1 in ANSI REXX */
parse arg file
if file = '' then file = 'day3.txt'
map. = ' '
closest = 999999999

do while lines(file)
	line = linein(file)
	x = 0
	y = 0
	call read
end
say closest
exit

read:
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
	call plot x, y, dx, dy, steps
	x = x + steps * dx
	y = y + steps * dy
end
return

plot: procedure expose map. closest
arg x, y, dx, dy, steps
do steps
	x = x + dx
	y = y + dy
	if map.x.y = '.'
	then do
		closest = min(closest, abs(x) + abs(y))
		map.x.y = 'X'
	end
	else map.x.y = '.'
end
return

bad_input:
say 'Bad input:' direction
exit 1
file = 'day3.txt'
map. = ' '
closest = 999999999
n = 0; e = 0; s = 0; w = 0
do while lines(file)
	line = linein(file)
	x = 0
	y = 0
	call f
end
exit

f:
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
	/*
	call show
	*/
	x = x + steps * dx
	y = y + steps * dy
	say direction steps dx dy x y
	n = min(y, n)
	s = max(y, s)
	w = min(x, w)
	e = max(x, e)
end

say x y
say n s e w
say closest
return
exit

plot: procedure expose map. closest
arg x, y, dx, dy, steps
say x y dx dy steps
say x + dx * steps
say y + dy * steps
_x = x + dx * steps
_y = y + dy * steps
say '_x:' _x '_y:' _y
do steps
	x = x + dx
	y = y + dy
	say 'x:' x 'y:' y
	if map.x.y = '.'
	then do
		closest = min(closest, abs(x) + abs(y))
		map.x.y = 'X'
	end
	else map.x.y = '.'
end
return

show: procedure expose map. n s e w
do y = n to s
	row = ''
	do x = w to e
		row = row || map.x.y
	end
	say row
end
return
return

bad_input:
say 'Bad input:' direction
exit 1
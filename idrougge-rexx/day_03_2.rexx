file = 'day3.txt'
map. = ' '
closest = 999999999
min_steps = 999999999
n = 0; e = 0; s = 0; w = 0
do while lines(file)
	line = linein(file)
	total_steps = 0
	call f
end
exit

f:
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
	call plot x, y, dx, dy, steps, total_steps
	call show
	x = x + steps * dx
	y = y + steps * dy
	total_steps = total_steps + steps
	say direction steps dx dy x y
	n = min(y, n)
	s = max(y, s)
	w = min(x, w)
	e = max(x, e)
end

say x y
say n s e w
say closest min_steps
return
exit

plot: procedure expose map. closest min_steps
arg x, y, dx, dy, steps, total_steps
say x y dx dy steps
say x + dx * steps
say y + dy * steps
do i = 1 to steps
	x = x + dx
	y = y + dy
	say 'x:' x 'y:' y
	/* if map.x.y = '.' */
	if map.x.y.z \= ' '
	then do
		map.x.y.z = map.x.y.z + total_steps + i
		if map.x.y.z < min_steps then do
			min_steps = map.x.y.z
		end
		map.x.y = 'X'
	end
	else do
		map.x.y = '.'
		map.x.y.z = total_steps + i
	end
end
return

show: procedure expose map. n s e w
do y = n to s
	row = ''
	do x = w to e
		select
			when x = 0 & y = 0
				then row = row || centre('O', 4)
			when map.x.y = 'X'
				then row = row || centre(map.x.y.z, 4)
			otherwise 
				row = row || centre(map.x.y.z, 4)
		end
	end
	say row
end
return
return

bad_input:
say 'Bad input:' direction
exit 1
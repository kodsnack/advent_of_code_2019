/* Advent of code 2019, day 4, part 2 in ANSI REXX */
parse arg input
if input = '' then input = '134564-585159'
parse var input from '-' til
count = 0

last = from
do i = from to til
	number = i
	match = 0
	parse var number n +1 number
	do until number = ''
		parse var number o +1
		if o < n then iterate i
		if n = o then match = 1
		parse var number n +1 number
	end
	count = count + match
end

say count
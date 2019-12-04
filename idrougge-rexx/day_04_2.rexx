input = '134564-585159'
parse var input from '-' til
say from til
count = 0

last = from
do i = from to til
	number = i
	match = 0
	parse var number n +1 number
	do until number = ''
		parse var number o +1
		if o < n then iterate i
		if abbrev(number, n||n) then do 
			do until left(number, 1) \= o
				parse var number n +1 number
			end
			iterate
		end
		if n = o then match = 1
		parse var number n +1 number
	end
	if match then say i match
	count = count + match
end

say count
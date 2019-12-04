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
		if n = o then match = 1
		/*
		if n = o then say i n '=' o
		if o < n then say o '<' n
		*/
		if o < n then iterate i
		parse var number n +1 number
	end
	say i
	count = count + match
end

say count
parse arg file
sum = 0
do while lines(file)
	mass = linein(file)
	sum = sum + mass % 3 - 2
end
say sum

/* Advent of code 2019, day 2, part 2 in ANSI REXX */
parse arg file
target_value = 19690720

/* Read input */
code = linein(file)
do # = 0 by 1 while code > ''
	parse var code rom.# ',' code
end

/* Step through all seed combinations from 0 to 99 until target is found */
do noun = 0 to 99 until result = target_value
	do verb = 0 to 99 until result = target_value
		call copymem
		ram.1 = noun
		ram.2 = verb
		result = calculate()
	end
end

say noun||verb
exit

/* Read opcode and src, src, dest operands */
calculate:
do pc = 0 to # by 4
	op = ram.pc

	src1 = pc + 1
	src1 = ram.src1

	src2 = pc + 2
	src2 = ram.src2

	dest = pc + 3
	dest = ram.dest

	select
		when op = 1 then ram.dest = ram.src1 + ram.src2
		when op = 2 then ram.dest = ram.src1 * ram.src2
		when op = 99 then return ram.0
		otherwise signal illegal
	end
end

copymem:
do i = 0 to #
	ram.i = rom.i
end
return

illegal:
say 'Illegal opcode:' op '@' pc
exit op
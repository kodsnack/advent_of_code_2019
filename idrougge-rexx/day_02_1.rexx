/* Advent of code 2019, day 2, part 1 in ANSI REXX */
parse arg file
code = linein(file)

do # = 0 by 1 while code > ''
	parse var code n ',' code
	memory.# = n
end

memory.1 = 12
memory.2 = 2

do pc = 0 to #
	op = memory.pc
	if op = 99 then leave
	pc = pc + 1
	o = memory.pc
	pc = pc + 1
	p = memory.pc
	pc = pc + 1
	dest = memory.pc
	select
		when op = 1 then memory.dest = memory.o + memory.p
		when op = 2 then memory.dest = memory.o * memory.p
		otherwise nop
	end
end

say memory.0
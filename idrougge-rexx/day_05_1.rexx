/* Advent of code 2019, day 5, part 1 in ANSI REXX */
parse arg file
if file = '' then file = 'day5.txt'

/* Push the given input to queue */
push 1

/* Read input */
code = linein(file)
do codesize = 0 by 1 while code > ''
	parse var code rom.codesize ',' code
end

call copymem
call calculate
exit

/* Read opcode and src, src, dest operands */
calculate:
pc = 0
do forever
	op = ram.pc

	parse value right(op, 5, 0) with mode.3 +1 mode.2 +1 mode.1 +1 op

	select
		when op = 1 then call add
		when op = 2 then call mul
		when op = 3 then call store
		when op = 4 then call load
		when op = 99 then return ram.0
		otherwise signal illegal
	end
end
return

add:
mode.3 = 1
parse value fetch(3) with src1 src2 dest .
ram.dest = src1 + src2
return

mul:
mode.3 = 1
parse value fetch(3) with src1 src2 dest .
ram.dest = src1 * src2
return

store:
mode.1 = 1
parse value fetch(1) with dest .
pull val
ram.dest = val
return

load:
parse value fetch(1) with src1 .
say src1
return

fetch: procedure expose ram. mode. pc
out = ''
do # = 1 to arg(1)
	@ = pc + #
	if mode.# then do
		out = out ram.@		
	end
	else do
		ref = ram.@
		out = out ram.ref
	end
end
pc = pc + #
return out

copymem:
do i = 0 to codesize
	ram.i = rom.i
end
return

illegal:
say 'Illegal opcode:' op '@' pc
exit op
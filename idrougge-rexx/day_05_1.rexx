/* Advent of code 2019, day 5, part 1 in ANSI REXX */
parse arg file
if file = '' then file = 'day5.txt'
target_value = 19690720

/* Read input */
push 1
code = '1002,4,3,4,33'
code = '1101,100,-1,4,0'
code = '3,5,4,5,99,-1'
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

	say '('pc'):' op
	say right(op, 5, 0)
	if right(op, 5, 0) \= translate(format(op, 5), 0, ' ') then exit 1
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
params = fetch(3)
say 'params:' params
parse var params src1 src2 dest .
say src1 '+' src2 '>' dest
ram.dest = src1 + src2
return

mul:
mode.3 = 1
params = fetch(3)
say 'params:' params
parse var params src1 src2 dest .
say src1 '*' src2 '>' dest
ram.dest = src1 * src2
return

store:
mode.1 = 1
parse value fetch(1) with dest .
say 'dest' dest
pull val
say 'val' val
say 'pc ->' pc
ram.dest = val
return

load:
parse value fetch(1) with src1 .
say 'load @'src1':' ram.src1
return

fetch: procedure expose ram. mode. pc
out = ''
do # = 1 to arg(1)
	say 'mode' #':' mode.#
	@ = pc + #
	if mode.# then do
		out = out ram.@		
	end
	else do
		ref = ram.@
		out = out ram.ref
	end
end
say '~'pc #
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
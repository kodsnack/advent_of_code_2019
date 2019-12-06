/* Advent of code 2019, day 5, part 1 in ANSI REXX */
parse arg file
if file = '' then file = 'day5.txt'
target_value = 19690720

/* Read input */
push 5
code = '1002,4,3,4,33'
code = '1101,100,-1,4,0'
code = '3,5,4,5,99,-1'
code = '3,9,8,9,10,9,4,9,99,-1,8'
code = '3,9,7,9,10,9,4,9,99,-1,8'
code = '3,3,1108,-1,8,3,4,3,99'
code = '3,3,1107,-1,8,3,4,3,99'
code = '3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9'
code = '3,3,1105,-1,9,1101,0,0,12,4,12,99,1'
code = '3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99'
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
		when op = 5 then call jnz
		when op = 6 then call jz
		when op = 7 then call clt
		when op = 8 then call cmp
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
say 'load:' src1
return

jnz:
parse value fetch(2) with val dest .
if val \= 0 then pc = dest
return

jz:
parse value fetch(2) with val dest .
if val = 0 then pc = dest
return

clt:
mode.3 = 1
parse value fetch(3) with src1 src2 dest .
ram.dest = src1 < src2
return

cmp:
mode.3 = 1
parse value fetch(3) with src1 src2 dest .
ram.dest = (src1 = src2)
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
/* Advent of code 2019, day 9, parts 1 & 2 in ANSI REXX */
parse arg file
if file = '' then file = 'day9.txt'

numeric digits 16

/* Read input */
code = linein(file)
do codesize = 0 by 1 while code > ''
	parse var code rom.codesize ',' code
end

/* Do parts 1 and 2 */
do i = 1 to 2
	/* Prove initial value */
	push i
	/* Initialise memory */
	call copymem
	/* Run program */
	call calculate
	/* Get answer returned on queue */
	pull answer
	say answer
end
exit

/* Read opcode */
calculate:
relative_base = 0
pc = 0
do forever
	op = ram.pc

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
		when op = 9 then call adj
		when op = 99 then return ram.0
		otherwise signal illegal
	end

	pc = pc + 1
end
return

add:
parse value fetch(2) with src1 src2 .
call save (src1 + src2), mode.3
return

mul:
parse value fetch(2) with src1 src2 .
call save (src1 * src2), mode.3
return

store:
pull val
call save val, mode.1
return

load:
parse value fetch(1) with src1 .
push src1
return

jnz:
parse value fetch(2) with val dest .
if val \= 0 then pc = dest - 1
return

jz:
parse value fetch(2) with val dest .
if val = 0 then pc = dest - 1
return

clt:
parse value fetch(2) with src1 src2 .
call save (src1 < src2), mode.3
return

cmp:
parse value fetch(2) with src1 src2 .
call save (src1 = src2), mode.3
return

adj:
parse value fetch(1) with val .
relative_base = relative_base + val
return

save: procedure expose ram. pc relative_base
arg val, mode
pc = pc + 1
@ = pc
dest = ram.@
if mode = 2 then dest = dest + relative_base
ram.dest = val
return out

fetch: procedure expose ram. mode. pc relative_base
out = ''
do # = 1 for arg(1)
	@ = pc + #
	select
		when mode.# = 1 then out = out ram.@
		when mode.# = 2 then do
			ref = ram.@
			ref = relative_base + ref
			out = out ram.ref
		end
		when mode.# = 0 then do
			ref = ram.@
			out = out ram.ref
		end
	end
end
pc = pc + arg(1)
return out

copymem:
drop ram.
ram. = 0
do p = 0 for codesize
	ram.p = rom.p
end
return

illegal:
say 'Illegal opcode:' op '@' pc
exit op
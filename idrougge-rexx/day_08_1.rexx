/* Advent of code 2019, day 8, part 1 in ANSI REXX */
parse arg file
if file = '' then file = 'day8.txt'
input = linein(file)
w = 25
h = 6
size = w * h
number_of_layers = length(input) / size
least_zeros = size

do i = 0 for number_of_layers
	layer = substr(input, size * i + 1, size)
	zeros = countstr(0, layer)
	if zeros < least_zeros then do
		least_zeros = zeros
		answer = countstr(1, layer) * countstr(2, layer)
	end
end

say answer

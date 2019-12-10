/* Advent of code 2019, day 8, part 2 in ANSI REXX */
parse arg file
if file = '' then file = 'day8.txt'
input = linein(file)
w = 25
h = 6
size = w * h
number_of_layers = length(input) / size
image = copies(' ', size)

do i = 0 for number_of_layers
	layer = substr(input, size * i + 1, size)
	layer = translate(layer, '01', '012')
	do @ = 1 to size
		if substr(image, @, 1) == ' ' then image = overlay(substr(layer, @), image, @, 1)
	end
end

image = translate(image, '* ', '10')
do i = 0 for h
	say substr(image, w * i + 1, w)
end

set the_file to choose file
set sum to 0
repeat with mass in paragraphs of (read the_file)
	set fuel to mass div 3 - 2
	set sum to sum + fuel
end repeat
return sum
set the_file to choose file
set sum to 0
repeat with mass in paragraphs of (read the_file)
	set sum to sum + (calculate_fuel for mass)
end repeat
return sum

on calculate_fuel for mass
	set fuel to mass div 3 - 2
	if fuel is greater than 0 then
		return fuel + (calculate_fuel for fuel)
	else
		return 0
	end if
end calculate_fuel
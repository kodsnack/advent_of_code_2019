parse arg file
sum = 0
do while lines(file)
	mass = fuel(linein(file))
	sum = sum + mass
end
say sum
exit

fuel: procedure
arg mass
mass = mass % 3 - 2
if mass > 0 then return mass + fuel(mass)
return 0
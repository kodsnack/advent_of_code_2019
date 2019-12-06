file_content = open("1dec-input", 'r')

fuel = 0

for line in file_content:
    mass = int(line)
    unit_fuel = (mass - (mass % 3)) / 3 - 2
    fuel += unit_fuel

print("Fuel needed step 1: %d" % fuel)

file_content.close()

file_content = open("1dec-input", 'r')
fuel = 0

for line in file_content:
    mass = int(line)
    unit_fuel = (mass - (mass % 3)) / 3 - 2
    fuel += unit_fuel

    while(unit_fuel > 8):
        unit_fuel = (unit_fuel - (unit_fuel % 3)) / 3 - 2
        fuel += unit_fuel

print("Fuel needed step 2: %d" % fuel)

file_content.close()

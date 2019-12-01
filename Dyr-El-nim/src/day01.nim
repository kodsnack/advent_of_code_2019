import strutils

proc fuelForMass*(mass: int): int =
    result = mass div 3 - 2

proc solution1*(file: File): int =
    for line in file.lines:
        let
            mass = line.parseInt
        result.inc(fuelForMass(mass))

proc fuelForAllMass*(mass: int): int =
    let
        thisMass = fuelForMass(mass)
    if thisMass > 0:
        result = thisMass + fuelForAllMass(thisMass)

proc solution2*(file: File): int =
    for line in file.lines:
        let
            mass = line.parseInt
        result.inc(fuelForAllMass(mass))
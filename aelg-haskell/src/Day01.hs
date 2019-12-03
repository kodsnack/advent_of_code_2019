module Day01
    ( solve
    )
where

import           Control.Arrow

fuelMass = subtract 2 . (`div` 3)

fuelMassWithFuel = sum . takeWhile (> 0) . iterate fuelMass

parse = map read

solve1 = show . sum . map fuelMass

solve2 = show . sum . map (fuelMassWithFuel . fuelMass)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2


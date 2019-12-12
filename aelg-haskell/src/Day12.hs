module Day12
    ( solve
    )
where

import           Control.Arrow
import           Data.Monoid
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

data Planet = Planet Int Int Int Int Int Int deriving Show

planet = do
    string "<x="
    x <- P.integer
    string ", y="
    y <- P.integer
    string ", z="
    z <- P.integer
    string ">"
    eof
    return $ Planet x y z 0 0 0

parse = map (P.run planet)

sign n | n < 0     = -1
       | n > 0     = 1
       | otherwise = 0

gravity (Planet x1 y1 z1 vx1 vy1 vz1) (Planet x2 y2 z2 vx2 vy2 vz2) = Planet
    x1
    y1
    z1
    vvx1
    vvy1
    vvz1
  where
    vvx1 = vx1 + sign (x2 - x1)
    vvy1 = vy1 + sign (y2 - y1)
    vvz1 = vz1 + sign (z2 - z1)

gravity1 (x1, vx1) (x2, vx2) = (x1, vvx1) where vvx1 = vx1 + sign (x2 - x1)

move (Planet x1 y1 z1 vx1 vy1 vz1) =
    Planet (x1 + vx1) (y1 + vy1) (z1 + vz1) vx1 vy1 vz1

move1 (x1, vx1) = (x1 + vx1, vx1)

systemGravity ps = map go ps where go p = foldl gravity p ps

systemGravity1 ps = map go ps where go p = foldl gravity1 p ps

systemMove = map move

systemMove1 = map move1

stepSystem = systemMove . systemGravity

stepSystem1 = systemMove1 . systemGravity1

energy (Planet x1 y1 z1 vx1 vy1 vz1) =
    (abs x1 + abs y1 + abs z1) * (abs vx1 + abs vy1 + abs vz1)

systemEnergy = getSum . foldMap (Sum . energy)

getX (Planet x1 _ _ vx1 _ _) = (x1, vx1)

getY (Planet _ x1 _ _ vx1 _) = (x1, vx1)

getZ (Planet _ _ x1 _ _ vx1) = (x1, vx1)

--solve1 :: [String] -> String
solve1 = show . systemEnergy . (!! 1000) . iterate stepSystem

period xs =
    (+ 1) . length . takeWhile (/= xs) . drop 1 . iterate stepSystem1 $ xs

--solve2 :: [String] -> String
solve2 ps = show $ lcm (lcm xs ys) zs
  where
    xs = period (map getX ps)
    ys = period (map getY ps)
    zs = period (map getZ ps)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

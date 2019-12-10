module Day10
    ( solve
    )
where

import           Control.Arrow
import           Data.List
import           Data.List.Index
import           Utils

parse = map (map (== '#'))

gSize g = (length . head $ g, length g)

inside g (x, y) = x < xM && x >= 0 && y < yM && y >= 0
    where (xM, yM) = gSize g

isAsteroid g (x, y) | inside g (x, y) = g !! y !! x
                    | otherwise       = False

genDirs (x, y) =
    nub . map simplify . filter (/= (0, 0)) $ (,) <$> [-x .. x] <*> [-y .. y]
    where simplify (x, y) = (x `div` a, y `div` a) where a = gcd x y

asteroids g = filter (isAsteroid g) $ (,) <$> [0 .. x] <*> [0 .. y]
    where (x, y) = gSize g

astInDir g (x, y) (dx, dy) | not (inside g (x + dx, y + dy)) = 0
                           | isAsteroid g (x + dx, y + dy) = 1
                           | otherwise = astInDir g (x + dx, y + dy) (dx, dy)

seenAsteroids g dirs (x, y) = ((x, y), sum . map (astInDir g (x, y)) $ dirs)

findStation g = maximumBy bySnd . map (seenAsteroids g dirs) $ asteroids g
  where
    dirs = genDirs (gSize g)
    bySnd a b = compare (snd a) (snd b)

solve1 = show . snd . findStation

asteroidInDirPos g (x, y) (dx, dy)
    | not (inside g (x + dx, y + dy)) = (0, 0)
    | isAsteroid g (x + dx, y + dy) = (x + dx, y + dy)
    | otherwise = asteroidInDirPos g (x + dx, y + dy) (dx, dy)

laserTurn g _        []           = (g, [])
laserTurn g (sx, sy) (dir : dirs) = (returnedGrid, (x, y) : rest)
  where
    (returnedGrid, rest) = laserTurn newGrid (sx, sy) dirs
    newGrid              = modifyAt y (setAt x False) g
    (x, y)               = asteroidInDirPos g (sx, sy) dir
    destroyed            = (x, y) /= (0, 0)

laser g (sx, sy) dirs = rev ++ laser newGrid (sx, sy) dirs
    where (newGrid, rev) = laserTurn g (sx, sy) dirs

solve2 g = show $ ansX * 100 + ansY
  where
    (ansX  , ansY) = (!! 199) . filter (/= (0, 0)) . laser g (x, y) $ dirs
    ((x, y), _   ) = findStation g
    dirs           = sortBy angleSort $ genDirs (gSize g)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

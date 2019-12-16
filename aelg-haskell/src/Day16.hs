module Day16
    ( solve
    )
where

import           Control.Arrow
import           Data.Char
import           Data.List

parse :: [String] -> [Int]
parse = map (read . (: [])) . head

basePattern = [0, 1, 0, (-1)]

pattern n = drop 1 . cycle . concat . map (replicate n) $ basePattern

oneDigit x = abs x `rem` 10

fft :: [Int] -> [Int]
fft xs = zipWith (combine) (replicate (length xs) xs) (map pattern [1 ..])
    where combine as bs = oneDigit . sum $ zipWith (*) as bs

-- note: reverse twice below do be able to do strict scanl... way faster
ffft' :: [Int] -> [Int]
ffft' xs = scanl' go 0 xs where go a b = oneDigit (a + b)

ffft xs pos =
    take 8
        . reverse
        . (!! 100)
        . iterate ffft'
        . reverse
        . drop (pos)
        . concat
        . replicate 10000
        $ xs

solve1 xs =
    concat . map show . take 8 . (!! 100) . iterate fft $ xs

solve2 xs = concat . map show $ ffft xs pos
    where pos = read . concat . map show . take 7 $ xs

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

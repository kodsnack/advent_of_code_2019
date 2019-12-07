module Day03
    ( solve
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Maybe
import           Data.Char
import           Text.ParserCombinators.ReadP
import qualified Parsing                       as P
import qualified Utils                         as U

data Dir = Dir Char Int deriving Show

type Pos = (Int, Int)
type StepPos = (Int, Pos)

wDir = do
    d <- char 'R' <++ char 'L' <++ char 'U' <++ char 'D'
    Dir d <$> P.integer

wire = do
    r <- P.listOf wDir
    eof
    return r

parse = map (P.run wire)

genPos :: StepPos -> [Dir] -> [StepPos]
genPos (_    , (x, y)) []             = []
genPos (steps, (x, y)) (Dir d i : xs) = l ++ genPos (last l) xs
  where
    l = go steps d (x, y) (i + 1)
    go _ _ _ 0 = []
    go steps 'R' (x, y) i =
        (steps, (x, y)) : go (steps + 1) 'R' (x + 1, y) (i - 1)
    go steps 'L' (x, y) i =
        (steps, (x, y)) : go (steps + 1) 'L' (x - 1, y) (i - 1)
    go steps 'U' (x, y) i =
        (steps, (x, y)) : go (steps + 1) 'U' (x, y + 1) (i - 1)
    go steps 'D' (x, y) i =
        (steps, (x, y)) : go (steps + 1) 'D' (x, y - 1) (i - 1)

getPos = snd
getSteps = fst

manhattan (x, y) = abs x + abs y
sortPos = sortOn manhattan

manhattanCmp a b = compare (manhattan a) (manhattan b)
manhattanEq a b = manhattan a == manhattan b

groupPos = groupBy manhattanEq


firstIntersection (a : as, b : bs)
    | or $ (==) <$> a <*> b                = manhattan . head $ a
    | manhattanCmp (head a) (head b) == EQ = firstIntersection (as, bs)
    | manhattanCmp (head a) (head b) == LT = firstIntersection (as, b : bs)
    | manhattanCmp (head a) (head b) == GT = firstIntersection (a : as, bs)

solve1 :: [[Dir]] -> String
solve1 = show . firstIntersection . toPair . map
    (tail . groupPos . sortPos . map getPos . genPos (0, (0, 0)))
    where toPair [a, b] = (a, b)

findShortest :: Int -> ([StepPos], [StepPos]) -> Int
findShortest depth (a, b) | depth > 2 * (length a + length b) = -1
                          | null shortest = findShortest (depth * 2) (a, b)
                          | head shortest < depth = head shortest
                          | otherwise = findShortest (depth * 2) (a, b)
  where
    shortest =
        map len
            .   sortOn len
            .   filter isEqual
            $   (,)
            <$> take depth a
            <*> take depth b
    len (a, b) = getSteps a + getSteps b
    isEqual (a, b) = getPos a == getPos b

solve2 = show . findShortest 1 . toPair . map (tail . genPos (0, (0, 0)))
    where toPair [a, b] = (a, b)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2


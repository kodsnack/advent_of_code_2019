module Day13
    ( solve
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.List.Index
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                     as S
import           IntcodeVM
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP
import qualified Utils                        as U

chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

updateGrid m [x, y, a] = M.insert (x, y) a m

tile 0 = ' '
tile 1 = '#'
tile 2 = '%'
tile 3 = '='
tile 4 = '*'

tick (vm, m) input = (vm', m')
  where
    (output, vm') = runInOutVM input vm
    m'            = foldl updateGrid m (chunksOf 3 output)

findBallPaddle = M.foldrWithKey find (0, 0)
  where
    find (x, y) 4 (b, p) = (x, p)
    find (x, y) 3 (b, p) = (b, x)
    find k      _ b      = b

getScore m = show $ fromMaybe 0 (M.lookup (-1, 0) m)

paint m = getScore m ++ "\n" ++ U.paintGrid (80, 20) tile m

bestInput m = input b p
  where
    (b, p) = findBallPaddle m
    input b p | b < p     = [-1]
              | b > p     = [1]
              | otherwise = [0]

player (vm, m) = tick (vm, m) input where input = bestInput m

play vm = iterate player (vm, M.empty)

parse :: [String] -> [Int]
parse = P.run programParser . head

solve1 prog =
    show
        . length
        . filter (\[_, _, a] -> a == 2)
        . chunksOf 3
        . fst
        . getOutputs
        . runVM
        $ initVM [] prog

solve2 prog =
    getScore
        . snd
        . head
        . dropWhile (\(vm, _) -> not (isHalted vm))
        . play
        $ initial
    where initial = initVM [] (2 : drop 1 prog)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

module Day19
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

parse = P.run programParser . head

ask vm (x, y) = (== 1) . head . fst $ runInOutVM [x, y] vm

solve1 prog = show . length . filter id . concat $ U.genGrid (ask vm)
                                                             (0, 0, 49, 49)
    where vm = runVM $ initVM [] prog

walkTop f (x, y) | f (x + 1, y) = (x + 1, y)
                 | otherwise    = walkTop f (x, y + 1)

walkBottom f (x, y) | f (x, y + 1) = (x, y + 1)
                    | otherwise    = walkBottom f (x + 1, y)

findSquare ((tx, ty) : top) ((bx, by) : bottom)
    | tx < 100     = findSquare top ((bx, by) : bottom)
    | by < ty + 99 = findSquare ((tx, ty) : top) bottom
    | tx < bx + 99 = findSquare top ((bx, by) : bottom)
    | otherwise    = (bx, ty)

solve2 prog = show (x * 10000 + y)
  where
    top    = iterate (walkTop (ask vm)) (4, 4)
    bottom = iterate (walkBottom (ask vm)) (4, 4)
    vm     = runVM $ initVM [] prog
    (x, y) = findSquare top bottom

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

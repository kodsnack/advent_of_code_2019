module Day09
    ( solve
    )
where

import           Control.Arrow
import           Control.Lens
import           Data.List
import qualified Data.Map.Strict              as M
import           IntcodeVM
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

parse :: [String] -> [Int]
parse = P.run programParser . head

solve1 prog = show $ fst . getOutput . runVM $ initVM [1] prog

solve2 prog = show $ fst . getOutput . runVM $ initVM [2] prog

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

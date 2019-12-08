module Template
    ( solve
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                     as S
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP
import qualified Utils                        as U

parse = id

--solve1 :: [String] -> String
solve1 = unlines

--solve2 :: [String] -> String
solve2 = solve1

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

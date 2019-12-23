module Day21
    ( solve
    )
where

import           Control.Arrow
import           Control.Lens
import           Data.Char
import           Data.List
import qualified Data.Map.Strict              as M
import           IntcodeVM
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

parse :: [String] -> [Int]
parse = P.run programParser . head

input1 = unlines ["OR D T", "NOT C J", "AND T J", "NOT A T", "OR T J", "WALK"]

-- This sucked stolen from somewhere
input2 = unlines
    [ "NOT A J"
    , "NOT C T"
    , "AND H T"
    , "OR T J"
    , "NOT B T"
    , "AND A T"
    , "AND C T"
    , "OR T J"
    , "AND D J"
    , "RUN"
    ]

solve1 prog =
    show . last . fst . runInOutVM (map ord input1) . runVM $ initVM [] prog

solve2 prog =
    show . last . fst . runInOutVM (map ord input2) . runVM $ initVM [] prog

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

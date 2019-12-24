module SolutionLookup
    ( solutions
    )
where

import qualified Data.Map as M

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24

solutions = M.fromList
    [ (1 , Day01.solve)
    , (2 , Day02.solve)
    , (3 , Day03.solve)
    , (4 , Day04.solve)
    , (5 , Day05.solve)
    , (6 , Day06.solve)
    , (7 , Day07.solve)
    , (8 , Day08.solve)
    , (9 , Day09.solve)
    , (10, Day10.solve)
    , (11, Day11.solve)
    , (12, Day12.solve)
    , (13, Day13.solve)
    , (14, Day14.solve)
    , (15, Day15.solve)
    , (16, Day16.solve)
    , (17, Day17.solve)
    , (18, Day18.solve)
    , (19, Day19.solve)
    , (20, Day20.solve)
    , (21, Day21.solve)
    , (22, Day22.solve)
    , (23, Day23.solve)
    , (24, Day24.solve)
    ]

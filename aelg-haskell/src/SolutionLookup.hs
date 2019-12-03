module SolutionLookup
    ( solutions
    )
where

import qualified Data.Map                      as M

import qualified Day01
import qualified Day02
import qualified Day03

solutions = M.fromList
    [(1, Day01.solve), (2, Day02.solve), (3, Day03.solve)]

module Day05
    ( solve
    )
where

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict              as M
import           IntcodeVM
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP


parse :: [String] -> [Int]
parse = P.run programParser . head

runProgram inputs = view outputs . runVM . initVM inputs

solve1 :: [Int] -> String
solve1 l = show . last $ runProgram [1] l

--solve2 :: [String] -> String
solve2 l = show . last $ runProgram [5] l

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

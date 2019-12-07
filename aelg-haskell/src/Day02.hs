module Day02
    ( solve
    )
where

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict               as M
import           Text.ParserCombinators.ReadP
import qualified Parsing                       as P
import           IntcodeVM

parse :: [String] -> [Int]
parse = P.run programParser . head

runProgram program noun verb = head . view memory . runVM $ initVM
    []
    (head program : noun : verb : drop 3 program)

solve1 :: [Int] -> String
solve1 l = show $ runProgram l 12 2

solve2 l = show (noun * 100 + verb)
  where
    ((noun, verb), _) =
        head
            . dropWhile ((/= 19690720) . snd)
            $ ((\n v -> ((n, v), runProgram l n v)) <$> [1 .. 99] <*> [1 .. 99])

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

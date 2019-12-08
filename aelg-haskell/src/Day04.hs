module Day04
    ( solve
    )
where

import           Control.Arrow
import           Data.List
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

range = (,) <$> P.integerAnd (char '-') <*> P.integerAnd eof

parse = P.run range . head

adjacent :: String -> Bool
adjacent = any ((> 1) . length) . group

adjacentEq :: String -> Bool
adjacentEq = any ((== 2) . length) . group

increasing [a] = True
increasing (a : b : xs) | b < a     = False
                        | otherwise = increasing (b : xs)

solve1 (a, b) =
    show
        . length
        . filter ((== 6) . length)
        . filter increasing
        . filter adjacent
        . map show
        $ [a .. b]

solve2 (a, b) =
    show
        . length
        . filter ((== 6) . length)
        . filter increasing
        . filter adjacentEq
        . map show
        $ [a .. b]

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

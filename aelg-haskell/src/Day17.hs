module Day17
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

getField prog = lines . init . map chr $ field
    where (field, vm) = runInOutVM [] (initVM [] prog)

isScaffold g (x, y) | x < 0 || y < 0 || x >= xm || y >= ym = False
                    | g !! y !! x `elem` "#<>^v"           = True
                    | otherwise                            = False
    where (xm, ym) = (length . head $ g, length g)

isIntersection g (x, y)
    | all (isScaffold g)
          [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    = x * y
    | otherwise
    = 0

solve1 prog = show . sum . map sum $ U.genGrid (isIntersection field)
                                               (0, 0, xm, ym)
  where
    field    = getField prog
    (xm, ym) = (length . head $ field, length field)

subA = "R,4,L,12,L,8,R,4\n"
subB = "L,8,R,10,R,10,R,6\n"
subC = "R,4,R,10,L,12\n"

mainProg = "A,B,A,C,A,B,A,C,B,C\n"

input = mainProg ++ subA ++ subB ++ subC ++ "n\n"

solve2 prog = show $ last out
    where (out, vm) = runInOutVM (map ord input) (initVM [] (setAt 0 2 prog))

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

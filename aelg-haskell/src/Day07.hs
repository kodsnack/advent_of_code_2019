module Day07
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

initVMs program = map (runVM . init)
    where init phase = initNamedVM ("phase " ++ show phase) [phase] program

runPass :: [VM] -> Int -> (Int, [VM])
runPass vms i = foldl' runAmp (i, []) vms
  where
    runAmp (i, vms) vm = (out, vms ++ [nextVM])
        where (out, nextVM) = getOutput . runVM $ addInputs [i] vm

feedbackLoop :: Int -> [VM] -> (Int, [VM])
feedbackLoop input vms | all isHalted vms = (input, vms)
                       | all isHalted vms = error "All vms not halted"
                       | otherwise        = feedbackLoop i newVMs
    where (i, newVMs) = runPass vms input

maxOutput prog = maximum . map runPhase . permutations
    where runPhase phase = fst $ feedbackLoop 0 (initVMs prog phase)

parse :: [String] -> [Int]
parse = P.run programParser . head

solve1 l = show $ maxOutput l [0 .. 4]

solve2 l = show $ maxOutput l [5 .. 9]

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

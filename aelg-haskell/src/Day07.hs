module Day07
    ( solve
    )
where

import           Control.Arrow
import           Control.Lens
import           Data.List
import qualified Data.Map.Strict               as M
import           Text.ParserCombinators.ReadP
import qualified Parsing                       as P
import           IntcodeVM


parse :: [String] -> [Int]
parse = P.run programParser . head

runAmplifiers :: [Int] -> Int -> [Int] -> Int
runAmplifiers program i []               = i
runAmplifiers program i (phase : phases) = runAmplifiers program output phases
  where
    vm     = initVM [phase, i] program
    output = head $ view outputs (runVM vm)

solve1 :: [Int] -> String
solve1 l = show . maximum . map (runAmplifiers l 0) $ permutations [0 .. 4]

initVMs program = map (runVM . init) where init phase = initVM [phase] program

runPass :: [VM] -> Int -> (Int, [VM])
runPass vms i = foldl' runAmp (i, []) vms
  where
    runAmp (i, vms) vm@WaitingVM{} = (out, vms ++ [nextVM])
        where (out, nextVM) = getOutputVM . runVM $ addInputsVM [i] vm

feedbackLoop :: Int -> [VM] -> (Int, [VM])
feedbackLoop input vms | all isHalted vms = (input, vms)
                       | all isHalted vms = error "All vms not halted"
                       | otherwise        = feedbackLoop i newVMs
  where
    (i, newVMs) = runPass vms input
    isHalted :: VM -> Bool
    isHalted HaltedVM{} = True
    isHalted _          = False

solve2 l = show . maximum . map runPhase $ permutations [5 .. 9]
    where runPhase phase = fst $ feedbackLoop 0 (initVMs l phase)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

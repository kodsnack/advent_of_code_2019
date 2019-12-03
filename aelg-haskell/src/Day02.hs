{-# LANGUAGE TemplateHaskell #-}

module Day02
  ( solve
  )
where

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict               as M
import           Text.ParserCombinators.ReadP
import qualified Parsing                       as P

type Memory = [Int]
type Instr = [Int]
type InstructionSet = M.Map Int Op
data VM = VM { _memory :: [Int]
             , _instructionPointer :: Int
             , _instructionSet :: InstructionSet} | HaltedVM {_memory :: [Int]
}
data Op = Op {runOp :: Instr -> VM -> VM, instructionLength :: Int}

$(makeLenses ''VM)

stepVM :: VM -> VM
stepVM (VM m i is) = runOp op instruction $ VM m (i + instructionLength op) is where
  opCode      = m !! i
  op          = is M.! opCode
  instruction = drop 1 . take (instructionLength op) . snd . splitAt i $ m


updateMem pos new l = a ++ [new] ++ b where (a, _ : b) = splitAt pos l

add [a, b, c] m = updateMem c (m !! a + m !! b) m
multiply [a, b, c] m = updateMem c (m !! a * m !! b) m

operation len f = Op { runOp = \i -> memory %~ f i, instructionLength = len }

vmInstructionSet = M.fromList
  [ (1 , operation 4 add)
  , (2 , operation 4 multiply)
  , (99, Op { runOp = \i vm -> HaltedVM (_memory vm), instructionLength = 1 })
  ]

initVM mem noun verb = VM { _memory = updateMem 2 verb (updateMem 1 noun mem)
                          , _instructionPointer = 0
                          , _instructionSet = vmInstructionSet
                          }

parse :: [String] -> [Int]
parse = P.run parser . head
 where
  parser = do
    h <- P.integer
    l <- many (char ',' >> P.integer)
    eof
    return $ h : l

runVM :: VM -> VM
runVM vm@VM{}                     = runVM . stepVM $ vm
runVM vm@HaltedVM { _memory = m } = vm

runProgram program noun verb =
  head . view memory . runVM $ initVM program noun verb

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

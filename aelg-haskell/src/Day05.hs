{-# LANGUAGE TemplateHaskell #-}

module Day05
  ( solve
  )
where

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict               as M
import           Text.ParserCombinators.ReadP
import qualified Parsing                       as P
import           Debug.Trace

type Memory = [Int]
data OpMode = PosMode | ImMode deriving Show
data OpCode = OpCode Int [OpMode] deriving Show
data Instr = Instr OpCode [Int]
type InstructionSet = M.Map Int Op
data VM = VM { _memory :: [Int]
             , _instructionPointer :: Int
             , _instructionSet :: InstructionSet
             , _outputs :: [Int]
             , _inputs :: [Int]
          } |
          HaltedVM { _memory :: [Int]
                   , _outputs :: [Int]
          }
data Op = Op {runOp :: Instr -> VM -> VM, instructionLength :: Int}

$(makeLenses ''VM)

parseInstruction i = OpCode opCode modes
 where
  opCode = i `rem` 100
  modes  = map mode . take 4 $ go [] (i `div` 100) ++ repeat 0
  mode 0 = PosMode
  mode 1 = ImMode
  go xs 0 = xs
  go xs x = go (xs ++ [x `rem` 10]) (x `div` 10)

stepVM :: VM -> VM
stepVM vm@VM { _memory = m, _instructionPointer = i, _instructionSet = is } =
  runOp op instruction (vm { _instructionPointer = i + instructionLength op })
 where
  OpCode opCode mode = parseInstruction (m !! i)
  op                 = is M.! opCode
  instruction        = Instr
    (OpCode opCode mode)
    (drop 1 . take (instructionLength op) . snd . splitAt i $ m)

add (a : b : _) (_ : _ : c : _) = memory %~ c (a + b)
multiply (a : b : _) (_ : _ : c : _) = memory %~ c (a * b)

lessThan (a : b : _) (_ : _ : c : _) | a < b     = memory %~ c 1
                                     | otherwise = memory %~ c 0
equals (a : b : _) (_ : _ : c : _) | a == b    = memory %~ c 1
                                   | otherwise = memory %~ c 0

jumpTo a = instructionPointer %~ const a
dontJump = id

jumpIfTrue (0 : _     : _) _ = dontJump
jumpIfTrue (_ : newIP : _) _ = jumpTo newIP

jumpIfFalse (0 : newIP : _) _ = jumpTo newIP
jumpIfFalse (_ : _     : _) _ = dontJump

input _ (a : _) vm = (inputs %~ drop 1) . (memory %~ a inputValue) $ vm
  where inputValue = head $ view inputs vm

output (a : _) _ = outputs %~ (++ [a])

updateList pos new l = a ++ [new] ++ b where (a, _ : b) = splitAt pos l

readInputs mem (Instr (OpCode _ opModes) args) = inputs
 where
  inputs = zipWith (curry getInput) opModes args
  getInput (PosMode, arg) = mem !! arg
  getInput (ImMode , arg) = arg

operation message len f = Op { runOp = run, instructionLength = len }
 where
  run instr@(Instr (OpCode opCode opModes) args) vm =
    maybeTraceInstr message instr . f inputs outputUpdate $ vm
   where
    inputs = readInputs (view memory vm) instr
    outputUpdate :: [Int -> [Int] -> [Int]]
    outputUpdate = zipWith outputFunction opModes args
    outputFunction :: OpMode -> Int -> (a -> [a] -> [a])
    outputFunction PosMode = updateList
    outputFunction mode =
      \_ -> error ("Illegal mode " ++ show mode ++ " in output instruction paramter")

vmInstructionSet = M.fromList
  [ (1, operation "add" 4 add)
  , (2, operation "multiply" 4 multiply)
  , (3, operation "outputs" 2 input)
  , (4, operation "outputs" 2 output)
  , (5, operation "jumpIfTrue" 3 jumpIfTrue)
  , (6, operation "jumpIfFalse" 3 jumpIfFalse)
  , (7, operation "lessThan" 4 lessThan)
  , (8, operation "equals" 4 equals)
  , ( 99
    , Op { runOp             = \i vm -> HaltedVM (_memory vm) (_outputs vm)
         , instructionLength = 1
         }
    )
  ]

initVM inputs mem = VM { _memory             = mem
                       , _instructionPointer = 0
                       , _instructionSet     = vmInstructionSet
                       , _inputs             = inputs
                       , _outputs            = []
                       }

-- maybeTraceInstr message (Instr (OpCode _ opModes) args) =
--   trace (message ++ " " ++ show (opModes, args))
maybeTraceInstr message instr = id

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

runProgram inputs = view outputs . runVM . initVM inputs

solve1 :: [Int] -> String
solve1 l = show . last $ runProgram [1] l

--solve2 :: [String] -> String
solve2 l = show . last $ runProgram [5] l

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

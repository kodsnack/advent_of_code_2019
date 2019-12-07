{-# LANGUAGE TemplateHaskell #-}
module IntcodeVM
    ( VM(..)
    , programParser
    , initVM
    , runVM
    , getOutputVM
    , addInputsVM
    , outputs
    , memory
    )
where

import           Control.Arrow
import           Control.Lens
import           Data.List
import qualified Data.Map.Strict               as M
import           Text.ParserCombinators.ReadP
import qualified Parsing                       as P
import           Debug.Trace

type Memory = [Int]
data OpMode = PosMode | ImMode | IndirectMode deriving Show
data OpCode = OpCode Int [OpMode] deriving Show
data Instr = Instr OpCode [Int]
type InstructionSet = M.Map Int Op
data VM = VM { _memory :: [Int]
             , _instructionPointer :: Int
             , _instructionSet :: InstructionSet
             , _outputs :: [Int]
             , _inputs :: [Int]
          } |
          WaitingVM { _memory :: [Int]
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

-- VM implementation
programParser = do
    h <- P.integer
    l <- many (char ',' >> P.integer)
    eof
    return $ h : l

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

addInputsVM input vm = VM
    { _memory             = _memory vm
    , _instructionPointer = _instructionPointer vm
    , _instructionSet     = _instructionSet vm
    , _outputs            = _outputs vm
    , _inputs             = input
    }

getOutputVM vm = (head . view outputs $ vm, outputs %~ drop 1 $ vm)

output (a : _) _ = outputs %~ (++ [a])

updateList pos new l = a ++ [new] ++ b where (a, _ : b) = splitAt pos l

readInputs mem (Instr (OpCode _ opModes) args) = inputs
  where
    inputs = zipWith (curry getInput) opModes args
    getInput (PosMode     , arg) = mem !! arg
    getInput (ImMode      , arg) = arg
    getInput (IndirectMode, arg) = mem !! (mem !! arg)

operation message len f = Op {runOp = run, instructionLength = len}
  where
    run instr@(Instr (OpCode opCode opModes) args) vm =
        maybeTraceInstr message instr . f inputs outputUpdate $ vm
      where
        mem    = view memory vm
        inputs = readInputs mem instr
        outputUpdate :: [Int -> [Int] -> [Int]]
        outputUpdate = zipWith (outputFunction mem) opModes args
        outputFunction :: [Int] -> OpMode -> Int -> (a -> [a] -> [a])
        outputFunction mem PosMode      arg = updateList arg
        outputFunction mem IndirectMode arg = updateList (mem !! arg)
        outputFunction mem mode arg =
            \_ ->
                error
                    (  "Illegal mode "
                    ++ show mode
                    ++ " in output instruction paramter"
                    )

initVM inputs mem = VM
    { _memory             = mem
    , _instructionPointer = 0
    , _instructionSet     = vmInstructionSet
    , _inputs             = inputs
    , _outputs            = []
    }

runVM :: VM -> VM
runVM vm@VM{}        = runVM . stepVM $ vm
runVM vm@HaltedVM{}  = vm
runVM vm@WaitingVM{} = vm

traceInstr message (Instr (OpCode _ opModes) args) =
    trace (message ++ " " ++ show (opModes, args))
dontTraceInstr _ _ = id

maybeTraceInstr = dontTraceInstr

-- Instruction set
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

input _ (a : _) vm
    | null $ view inputs vm = WaitingVM
        { _memory             = _memory vm
        , _instructionPointer = _instructionPointer vm - 2
        , _instructionSet     = _instructionSet vm
        , _outputs            = _outputs vm
        , _inputs             = _inputs vm
        }
    | otherwise = (inputs %~ drop 1) . (memory %~ a inputValue) $ vm
    where inputValue = head $ view inputs vm

vmInstructionSet = M.fromList
    [ (1, operation "add" 4 add)
    , (2, operation "multiply" 4 multiply)
    , (3, operation "input" 2 input)
    , (4, operation "output" 2 output)
    , (5, operation "jumpIfTrue" 3 jumpIfTrue)
    , (6, operation "jumpIfFalse" 3 jumpIfFalse)
    , (7, operation "lessThan" 4 lessThan)
    , (8, operation "equals" 4 equals)
    , ( 99
      , Op
          { runOp             = \i vm -> HaltedVM (_memory vm) (_outputs vm)
          , instructionLength = 1
          }
      )
    ]

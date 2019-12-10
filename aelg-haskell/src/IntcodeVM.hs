{-# LANGUAGE TemplateHaskell #-}
module IntcodeVM
    ( VM(..)
    , programParser
    , initVM
    , initNamedVM
    , runVM
    , getOutput
    , getOutputs
    , addInputs
    , outputs
    , memory
    , VMState(..)
    , vmState
    , isHalted
    )
where

import           Control.Arrow
import           Control.Lens
import           Data.List
import qualified Data.Map.Strict              as M
import           Debug.Trace
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

type Memory = [Int]
data OpMode = PosMode | ImMode | RelMode  deriving Show
data OpCode = OpCode Int [OpMode] deriving Show
data Instr = Instr OpCode [Int]
type InstructionSet = M.Map Int Op
data VMState = VMExecuting | VMWaiting | VMHalted deriving Eq
data VM = VM { _memory             :: [Int]
             , _instructionPointer :: Int
             , _instructionSet     :: InstructionSet
             , _outputs            :: [Int]
             , _inputs             :: [Int]
             , _vmState            :: VMState
             , _vmName             :: String
             , _relBase            :: Int
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
    mode 2 = RelMode
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

continueVM vm | vm ^. vmState == VMHalted = error "Can't continue halted VM"
              | otherwise                 = vmState .~ VMExecuting $ vm

addInputs input = (inputs %~ (++ input)) . continueVM

getOutput vm = (head . view outputs $ vm, outputs %~ drop 1 $ vm)
getOutputs vm = (view outputs vm, outputs .~ [] $ vm)

output (a : _) _ = outputs %~ (++ [a])

updateList pos new l = a ++ [new] ++ b where (a, _ : b) = splitAt pos l

readInputs relBase mem (Instr (OpCode _ opModes) args) = inputs
  where
    inputs = zipWith (curry getInput) opModes args
    getInput (PosMode, arg) = mem !! arg
    getInput (ImMode , arg) = arg
    getInput (RelMode, arg) = mem !! (arg + relBase)

readOutputs :: Int -> [Int] -> Instr -> [Int -> VM -> VM]
readOutputs rBase mem (Instr (OpCode _ opModes) args) =
    map ((memory %~) .) $ zipWith (outputUpdater mem) opModes args
  where
    outputUpdater mem PosMode arg newValue = updateList arg newValue
    outputUpdater mem RelMode arg newValue = updateList (arg + rBase) newValue
    outputUpdater mem mode    arg a        = \_ -> error "Illegal mode"

isHalted vm = vm ^. vmState == VMHalted

type InstructionImplementation = [Int] -> [Int -> VM -> VM] -> (VM -> VM)
operation :: String -> Int -> InstructionImplementation -> Op
operation message len f = Op {runOp = run, instructionLength = len}
  where
    run instr vm =
        maybeTraceInstr name message ip instr inputs . f inputs outputs $! vm
      where
        name    = vm ^. vmName
        ip      = vm ^. instructionPointer
        mem     = vm ^. memory
        rBase   = vm ^. relBase
        inputs  = readInputs rBase mem instr
        outputs = readOutputs rBase mem instr

initNamedVM name inputs mem = VM
    { _memory             = mem ++ repeat 0
    , _instructionPointer = 0
    , _instructionSet     = vmInstructionSet
    , _inputs             = inputs
    , _outputs            = []
    , _vmState            = VMExecuting
    , _vmName             = name
    , _relBase            = 0
    }

initVM = initNamedVM "vm"

runVM :: VM -> VM
runVM vm | vm ^. vmState == VMExecuting = runVM . stepVM $ vm
         | otherwise                    = vm

traceInstr name message ip (Instr (OpCode _ opModes) args) inputs = trace
    (  name
    ++ ": "
    ++ message
    ++ " ip: "
    ++ pad 4 (show ip)
    ++ "  "
    ++ (pad 18 . unwords) operands
    ++ " values: "
    ++ inp
    )
  where
    operands = zipWith prettyOperand opModes args
    inp      = unwords . map (pad 8 . show) $ inputs
    pad n s = take n $ s ++ repeat ' '
    prettyOperand PosMode arg = pad 5 ('*' : show arg)
    prettyOperand ImMode  arg = pad 5 ('#' : show arg)
    prettyOperand RelMode arg = pad 5 ('~' : show arg)
dontTraceInstr _ _ _ _ _ = id

maybeTraceInstr = dontTraceInstr

-- Instruction set
add (a : b : _) (_ : _ : c : _) = c (a + b)
multiply (a : b : _) (_ : _ : c : _) = c (a * b)

lessThan (a : b : _) (_ : _ : c : _) | a < b     = c 1
                                     | otherwise = c 0
equals (a : b : _) (_ : _ : c : _) | a == b    = c 1
                                   | otherwise = c 0

jumpTo a = instructionPointer .~ a
dontJump = id

jumpIfTrue (0 : _     : _) _ = dontJump
jumpIfTrue (_ : newIP : _) _ = jumpTo newIP

jumpIfFalse (0 : newIP : _) _ = jumpTo newIP
jumpIfFalse (_ : _     : _) _ = dontJump

input _ (a : _) vm
    | null $ vm ^. inputs
    = (instructionPointer %~ subtract 2) . (vmState .~ VMWaiting) $ vm
    | otherwise
    = (inputs %~ drop 1) . a inputValue $ vm
    where inputValue = head $ vm ^. inputs

updateRelBase (a : _) _ = relBase %~ (+ a)

halt _ _ = vmState .~ VMHalted

vmInstructionSet = M.fromList
    [ (1 , operation "add" 4 add)
    , (2 , operation "mul" 4 multiply)
    , (3 , operation "inp" 2 input)
    , (4 , operation "out" 2 output)
    , (5 , operation "jTr" 3 jumpIfTrue)
    , (6 , operation "jFa" 3 jumpIfFalse)
    , (7 , operation "lt " 4 lessThan)
    , (8 , operation "eqs" 4 equals)
    , (9 , operation "rlb" 2 updateRelBase)
    , (99, operation "-- hlt -- " 1 halt)
    ]

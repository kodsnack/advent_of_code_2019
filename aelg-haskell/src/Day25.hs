module Day25
    ( solve
    )
where

import           Control.Arrow
import           Data.Char
import           IntcodeVM
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

parse :: [String] -> [Int]
parse = P.run programParser . head

input = unlines
    [ "west"
    , "take hologram"
    , "north"
    , "take space heater"
    , "east"
    , "take space law space brochure"
    , "west"
    , "south"
    , "east"
    , "east"
    , "east"
    , "east"
    , "take spool of cat6"
    , "west"
    , "west"
    , "south"
    , "east"
    , "east"
    , "east"
    , "south"
    ]

codeParser = do
    skipMany get
    string "You should be able to get in by typing "
    P.integerAnd (char ' ')

solve1 prog = show . P.run codeParser $ map chr output
    where (output, vm) = runInOutVM [] $ initVM (map ord input) prog

solve2 prog = "God jul!"

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

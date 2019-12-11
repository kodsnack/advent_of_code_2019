module Day11
    ( solve
    )
where

import           Control.Arrow
import           Data.List
import           Data.List.Index
import qualified Data.Map.Strict              as M
import           IntcodeVM
import qualified Parsing                      as P

dirs n = [(0, -1), (1, 0), (0, 1), (-1, 0)] !! (n `rem` 4)

runRobot defColor vm (x, y) dir tiles
    | isHalted nextVM = updatedTiles
    | otherwise       = runRobot 0 nextVM (x + dx, y + dy) newDir updatedTiles
  where
    ([color, turn], nextVM) = getOutputs . runVM . addInputs [curTile] $ vm
    curTile                 = M.findWithDefault defColor (x, y) tiles
    updatedTiles            = M.insert (x, y) color tiles
    newDir                  = 4 + dir + (2 * turn - 1)
    (dx, dy)                = dirs newDir


paint :: M.Map (Int, Int) Int -> String
paint m = unlines . map (intersperse ' ') $ [] : M.foldrWithKey line grid m
  where
    line (x, y) c = modifyAt y (char x c)
    char x c = setAt x (color c)
    color 1 = '@'
    color 0 = ' '
    grid = replicate 6 (replicate 40 ' ')

parse :: [String] -> [Int]
parse = P.run programParser . head

solve1 prog = show $ M.size (runRobot 0 (initVM [] prog) (0, 0) 0 M.empty)

solve2 prog = paint (runRobot 1 (initVM [] prog) (0, 0) 0 M.empty)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

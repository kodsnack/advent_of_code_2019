module Day15
    ( solve
    )
where

import           Control.Arrow
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Semigroup
import           IntcodeVM
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP
import qualified Utils                        as U

parse = P.run programParser . head

data Path = Path VM (Int, Int)
instance Eq Path where
    (Path _ p1) == (Path _ p2) = p1 == p2
instance Ord Path where
    compare (Path _ p1) (Path _ p2) = compare p1 p2

data Step = OK Path | WALL | GOAL Path deriving (Eq, Ord)

type Pos = (Int, Int)

dirs = [(0, 0), (0, -1), (0, 1), (-1, 0), (1, 0)]

takeStep :: Path -> Int -> Step
takeStep (Path vm (x, y)) dir | out == 0 = WALL
                              | out == 1 = OK $ Path vm' (x + dx, y + dy)
                              | out == 2 = GOAL $ Path vm' (x + dx, y + dy)
  where
    ([out], vm') = runInOutVM [dir] vm
    (dx   , dy ) = dirs !! dir

genSteps' p = map (takeStep p) [1 .. 4]

genSteps :: Bool -> Step -> (Bool, [Step])
genSteps stop (GOAL p) = (stop, genSteps' p)
genSteps _    (OK   p) = (False, genSteps' p)
genSteps _    WALL     = (False, [])

solve1 prog = show . fromMaybe (-1) $ M.lookup goal m
  where
    vm = initVM [] prog
    (Just goal@(GOAL _), m) =
        U.bfsStoppable (genSteps True) [OK (Path vm (0, 0))]

solve2 prog = show . getMax $ foldMap Max oxygenM
  where
    vm = initVM [] prog
    (Just goal@(GOAL (Path svm pos)), m) =
        U.bfsStoppable (genSteps True) [OK (Path vm (0, 0))]
    (_, oxygenM) = U.bfsStoppable (genSteps False) [OK (Path svm pos)]

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

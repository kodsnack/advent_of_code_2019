module Day24
    ( solve
    )
where

import           Control.Arrow
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set        as S
import qualified Utils           as U

parse = id

data Bug = Bug | NoBug deriving (Show)
isBug Bug   = True
isBug NoBug = False

bug '#' = Bug
bug _   = NoBug

toChar Bug = '#'
toChar _   = '.'

score = M.foldlWithKey f 0
  where
    f a (x, y) NoBug = a
    f a (x, y) Bug   = a + 2 ^ (x + 5 * y)

grid xs = M.fromList . concat $ U.genGrid gen (0, 0, 4, 4)
    where gen (x, y) = ((x, y), bug (xs !! y !! x))

sumAdj f p m = length . filter isBug . catMaybes $ map (`M.lookup` m) (f p)

nextState dirs m = M.mapWithKey f m
  where
    f p Bug | sumAdj dirs p m == 1 = Bug
            | otherwise            = NoBug
    f p NoBug | sumAdj dirs p m == 1 || sumAdj dirs p m == 2 = Bug
              | otherwise = NoBug

printGrid m = unlines $ U.genGrid f (0, 0, 4, 4) where f p = toChar $ m M.! p

firstEqual s (x : xs) | S.member x s = x
                      | otherwise    = firstEqual (S.insert x s) xs

solve1 xs =
    show . firstEqual S.empty . map score $ iterate (nextState U.dirs) (grid xs)


bigGrid xs = M.filterWithKey middleFilter . foldl M.union M.empty $ map
    gen
    [-300 .. 300]
  where
    middleFilter (_, 2, 2) _ = False
    middleFilter _         _ = True
    gen 0 = M.fromList . concat $ U.genGrid
        (\(x, y) -> ((0, x, y), bug (xs !! y !! x)))
        (0, 0, 4, 4)
    gen level = M.fromList . concat $ U.genGrid
        (\(x, y) -> ((level, x, y), NoBug))
        (0, 0, 4, 4)

sameLevelDirs (l, x, y) = map (\(x, y) -> (l, x, y)) (U.dirs (x, y))

upperLevelDirs (l, x, y) = map (\(x, y) -> (l + 1, x, y))
                               (xds (x, y) ++ yds (x, y))
  where
    xds (0, _) = [(1, 2)]
    xds (4, _) = [(3, 2)]
    xds _      = []
    yds (_, 0) = [(2, 1)]
    yds (_, 4) = [(2, 3)]
    yds _      = []

lowerLevelDirs (l, 2, 1) =
    [(l - 1, 0, 0), (l - 1, 1, 0), (l - 1, 2, 0), (l - 1, 3, 0), (l - 1, 4, 0)]
lowerLevelDirs (l, 2, 3) =
    [(l - 1, 0, 4), (l - 1, 1, 4), (l - 1, 2, 4), (l - 1, 3, 4), (l - 1, 4, 4)]
lowerLevelDirs (l, 1, 2) =
    [(l - 1, 0, 0), (l - 1, 0, 1), (l - 1, 0, 2), (l - 1, 0, 3), (l - 1, 0, 4)]
lowerLevelDirs (l, 3, 2) =
    [(l - 1, 4, 0), (l - 1, 4, 1), (l - 1, 4, 2), (l - 1, 4, 3), (l - 1, 4, 4)]
lowerLevelDirs _ = []

dirs p = sameLevelDirs p ++ upperLevelDirs p ++ lowerLevelDirs p

solve2 xs = show . length . filter isBug . M.elems . (!! 200) $ iterate
    (nextState dirs)
    (bigGrid xs)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

{-# LANGUAGE BangPatterns #-}
module Day18
    ( solve
    , cheat
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.List.Index
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Set                     as S
import           Data.Tuple
import           Debug.Trace
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP
import qualified Utils                        as U

parse = filter ((> 1) . length)

data Tile = Open | Wall | Start | Door Char | Key Char deriving (Show, Eq)
isDoor (Door _) = True
isDoor _        = False
isKey (Key _) = True
isKey _       = False
isStart Start = True
isStart _     = False
door (Door d) = d
key (Key k) = [k]
key _       = ""

type Map = M.Map (Int, Int) Tile
data State = State !String ![(Int,Int)] deriving (Ord, Show)
instance Eq State where
    (State keys1 ps1) == (State keys2 ps2) = keys1 == keys2 && or (zipWith (==) ps1 ps2)

buildM xs = M.fromList . concat $ U.genGrid f (0, 0, mx - 1, my - 1)
  where
    (mx, my) = (length $ head xs, length xs)
    f (x, y) = ((x, y), tile (xs !! y !! x))
      where
        tile '#' = Wall
        tile '.' = Open
        tile '@' = Start
        tile c | isLower c = Key c
               | isUpper c = Door (toLower c)
               | otherwise = error $ show c

allKeys = sort . filter isLower . concat

getKeyPos :: M.Map (Int, Int) Tile -> S.Set (Int, Int)
getKeyPos = M.keysSet . M.filter isKey

getAllInteresting = M.keys . M.filter (\x -> isKey x || isDoor x || isStart x)

dirs (x, y) = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

solve1 xs = show . getMax . foldMap Max $ res
  where
    m      = buildM xs
    keyPos = getKeyPos m
    allks  = allKeys xs
    start  = M.keys $ M.filter isStart m
    paths  = buildPaths m
    (_, res) =
        U.dijkstra (dijkstraF1 paths allks keyPos m) [(0, ((mx, my), ""))]
    (mx, my) = (length (head xs) `div` 2, length xs `div` 2)

fixGrid =
    modifyAt 39 (setAt 39 '@' . setAt 40 '#' . setAt 41 '@')
        . modifyAt 40 (setAt 39 '#' . setAt 40 '#' . setAt 41 '#')
        . modifyAt 41 (setAt 39 '@' . setAt 40 '#' . setAt 41 '@')

buildPaths :: M.Map Pos Tile -> M.Map Pos [(Int, Pos)]
buildPaths m = M.fromList (map gen endpoints)
  where
    gen p = (p, interestingPaths p)
    endpoints = getAllInteresting m
    allPaths p = snd $ U.dijkstra (searcher (map ((,) 1) . dirs) "" m)
                                  (map ((,) 1) (dirs p))
    interestingPaths p = map swap . M.toList $ M.restrictKeys
        (allPaths p)
        (S.fromList endpoints)

searcher
    :: ((Int, Int) -> [(Int, (Int, Int))])
    -> String
    -> Map
    -> (Int, Int)
    -> (Bool, [(Int, (Int, Int))])
searcher nextSteps curKeys m (x, y)
    | t == Open || t == Start || isKey t  = (False, nextSteps (x, y))
    | isDoor t && (door t `elem` curKeys) = (False, nextSteps (x, y))
    | otherwise                           = (False, [])
    where t = fromMaybe (error ("bad tile" ++ show (x, y))) $ M.lookup (x, y) m

-- Sorry for all this mess.
nextStates
    :: M.Map Pos [(Int, Pos)]
    -> S.Set (Int, Int)
    -> M.Map (Int, Int) Tile
    -> String
    -> (Int, Int)
    -> [((Int, Int), (Int, Tile))]
nextStates paths keyPos m curKeys (x, y) = M.toList $ M.fromSet gen keyPos
  where
    gen (x, y) =
        ( fromMaybe maxBound $ M.lookup (x, y) keys
        , fromJust $ M.lookup (x, y) m
        )
    foo :: M.Map Pos [(Int, Pos)] -> Pos -> [(Int, Pos)]
    foo m p = fromMaybe [] (M.lookup p m)
    (_, keys) = U.dijkstra (searcher (foo paths) curKeys m) [(0, (x, y))]

type Pos = (Int, Int)
dijkstraF
    :: M.Map Pos [(Int, Pos)]
    -> String
    -> S.Set Pos
    -> M.Map Pos Tile
    -> ([Pos], String)
    -> (Bool, [(Int, ([Pos], String))])
dijkstraF paths allks keyPos m ([a, b, c, d], keys)
    | keys == allks = (True, next)
    | otherwise     = (False, next)
  where
    next =
        map nextA' nextA
            ++ map nextB' nextB
            ++ map nextC' nextC
            ++ map nextD' nextD
    nextA' :: (Int, (Pos, String)) -> (Int, ([Pos], String))
    nextA' (w, (a, s)) = (w, ([a, b, c, d], s))
    nextB' (w, (b, s)) = (w, ([a, b, c, d], s))
    nextC' (w, (c, s)) = (w, ([a, b, c, d], s))
    nextD' (w, (d, s)) = (w, ([a, b, c, d], s))
    nextA :: [(Int, (Pos, String))]
    nextA = map addKeys $ filter filt (nextStates paths keyPos m keys a)
    nextB = map addKeys $ filter filt (nextStates paths keyPos m keys b)
    nextC = map addKeys $ filter filt (nextStates paths keyPos m keys c)
    nextD = map addKeys $ filter filt (nextStates paths keyPos m keys d)
    addKeys (p, (x, Key k)) = (x, (p, sort (k : keys)))
    filt (_, (w, Key k)) = k `notElem` keys && w < 1000000000

dijkstraF1
    :: M.Map Pos [(Int, Pos)]
    -> String
    -> S.Set Pos
    -> M.Map Pos Tile
    -> (Pos, String)
    -> (Bool, [(Int, (Pos, String))])
dijkstraF1 paths allks keyPos m (a, keys) | keys == allks = (True, next)
                                          | otherwise     = (False, next)
  where
    next = map addKeys $ filter filt (nextStates paths keyPos m keys a)
    addKeys (p, (x, Key k)) = (x, (p, sort (k : keys)))
    filt (_, (w, Key k)) = k `notElem` keys && w < 1000000000

--solve2 :: [String] -> String
solve2 xs = show . getMax . foldMap Max $ res
  where
    m        = buildM (fixGrid xs)
    keyPos   = getKeyPos m
    allks    = allKeys xs
    start    = M.keys $ M.filter isStart m
    paths    = buildPaths m
    (_, res) = U.dijkstra
        (dijkstraF paths allks keyPos m)
        [ ( 0
          , ( [ (mx - 1, my - 1)
              , (mx + 1, my - 1)
              , (mx - 1, my + 1)
              , (mx + 1, my + 1)
              ]
            , ""
            )
          )
        ]
    (mx, my) = (length (head xs) `div` 2, length xs `div` 2)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2

cheat = const ("4544", "1692")

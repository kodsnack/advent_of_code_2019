{-# LANGUAGE TupleSections #-}
module Utils
    ( genGrid
    , bfs
    , parallel
    , angleSort
    , paintGrid
    , binarySearch
    )
where

import           Control.Parallel.Strategies
import           Data.List
import           Data.List.Index
import qualified Data.Map.Strict             as M
import           Data.Maybe
import qualified Queue                       as Q

genGrid f (minX, minY, maxX, maxY) = map (map f) (line <$> [minY .. maxY])
    where line y = map (, y) [minX .. maxX]


bfs :: Ord k => (k -> [k]) -> [k] -> M.Map k Int
bfs gen start = go (add start 0 Q.empty) M.empty
  where
    add k w = Q.pushList (zip (repeat w) k)
    go q seen
        | Q.null q = seen
        | M.member next seen = go tail seen
        | otherwise = go (add (gen next) (w + 1) tail) (M.insert next w seen)
        where (Just (w, next), tail) = Q.pop q


parallel :: (NFData a, Traversable f) => f a -> f a
parallel = (`using` parTraversable rdeepseq)

angleSort (x1, y1) (x2, y2) | x1 >= 0 && x2 < 0 = LT
                            | x2 >= 0 && x1 < 0 = GT
                            | x1 >= 0           = compare k2 k1
                            | otherwise         = compare k1 k2
  where
    k1 = fromIntegral y1 / fromIntegral x1
    k2 = fromIntegral y2 / fromIntegral x2

paintGrid :: (Int, Int) -> (a -> Char) -> M.Map (Int, Int) a -> String
paintGrid (xMax, yMax) f m =
    unlines . map (intersperse ' ') $ [] : M.foldrWithKey line grid m
  where
    line (x, y) c = modifyAt y (char x c)
    char x c = setAt x (f c)
    grid = replicate yMax (replicate xMax ' ')


-- binary Search finds x such that f(x) = True and f(x+1) = False
binarySearch f a b | a + 1 == b = a
                   | f guess    = binarySearch f guess b
                   | otherwise  = binarySearch f a guess
    where guess = (a + b) `div` 2

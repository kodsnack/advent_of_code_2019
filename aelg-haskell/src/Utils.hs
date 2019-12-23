{-# LANGUAGE TupleSections #-}
module Utils
    ( genGrid
    , bfs
    , bfsStoppable
    , dijkstra
    , parallel
    , angleSort
    , paintGrid
    , binarySearch
    , dirs
    )
where

import           Control.Parallel.Strategies
import           Data.List
import           Data.List.Index
import qualified Data.Map.Strict             as M
import           Data.Maybe
import qualified Data.Set                    as S
import           Debug.Trace
import qualified Queue                       as Q

genGrid f (minX, minY, maxX, maxY) = map (map f) (line <$> [minY .. maxY])
    where line y = map (, y) [minX .. maxX]

dirs (x, y) = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]


bfs :: Ord k => (k -> [k]) -> [k] -> M.Map k Int
bfs gen start = go (add start 0 Q.empty) M.empty
  where
    add k w = Q.pushList (zip (repeat w) k)
    go q seen
        | Q.null q = seen
        | M.member next seen = go tail seen
        | otherwise = go (add (gen next) (w + 1) tail) (M.insert next w seen)
        where (Just (w, next), tail) = Q.pop q

-- bfsStoppable can be stopped by returning True from then gen function
bfsStoppable :: Ord k => (k -> (Bool, [k])) -> [k] -> (Maybe k, M.Map k Int)
bfsStoppable gen start = go (add start 0 Q.empty) M.empty
  where
    add k w = Q.pushList (zip (repeat w) k)
    go q seen | Q.null q           = (Nothing, seen)
              | M.member next seen = go tail seen
              | done               = (Just next, M.insert next w seen)
              | otherwise = go (add new (w + 1) tail) (M.insert next w seen)
      where
        (Just (w, next), tail) = Q.pop q
        (done          , new ) = gen next

newtype DijkstraState a = DijkstraState (Int, a)
instance (Eq a) => Eq (DijkstraState a) where
  (DijkstraState (a1,a2)) == (DijkstraState (b1,b2)) = a1 == b1 && a2 == b2
instance (Eq a) => Ord (DijkstraState a) where
  (DijkstraState (a,_)) <= (DijkstraState (b,_)) = a <= b

dijkstra
    :: Ord k
    => (k -> (Bool, [(Int, k)]))
    -> [(Int, k)]
    -> (Maybe k, M.Map k Int)
dijkstra gen start = go (add start 0 S.empty) M.empty
  where
    add k w = pushList (map (DijkstraState . addWeight w) k)
    addWeight curW (w, k) = (curW + w, k)
    pushList xs q = S.union q (S.fromList xs)
    go q seen | S.null q           = (Nothing, seen)
              | M.member next seen = go tail seen
              | done               = (Just next, M.insert next w seen)
              | otherwise          = go (add new w tail) (M.insert next w seen)
      where
        (DijkstraState (w, next), tail) = S.deleteFindMin q
        (done                   , new ) = gen next

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

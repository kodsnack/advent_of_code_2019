module Day20
    ( solve
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                     as S
import           Debug.Trace
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP
import qualified Utils                        as U

type Pos = (Int, Int)
data Tile = Open | Wall | Portal String | ConnectedPortal Pos | Start | End deriving (Show, Eq)

parse = id

tile xs (x, y) | c == '.'                           = ((x, y), Open)
               | isAlpha a && isAlpha c && b == '.' = ((x, y), Portal [a, c])
               | isAlpha b && isAlpha c && a == '.' = ((x, y), Portal [c, b])
               | isAlpha d && isAlpha c && e == '.' = ((x, y), Portal [d, c])
               | isAlpha e && isAlpha c && d == '.' = ((x, y), Portal [c, e])
               | otherwise                          = ((x, y), Wall)
  where
    c = xs !! y !! x
    a = xs !! y !! (x - 1)
    b = xs !! y !! (x + 1)
    d = xs !! (y - 1) !! x
    e = xs !! (y + 1) !! x

buildM xs =
    M.mapWithKey connectPortals
        . fmap startEnd
        . M.fromList
        . filter notWall
        $ ts
  where
    ts = concat $ U.genGrid (tile xs) (1, 1, mx - 2, my - 2)
    mx = length . head $ xs
    my = length xs
    notWall (_, Wall) = False
    notWall _         = True
    connectPortals p (Portal s) = ConnectedPortal (findOther s p)
    connectPortals _ x          = x
    findOther s p = fst . head . filter (isOther s p) $ ts
    isOther s p (op, Portal os) = s == os && p /= op
    isOther _ _ _               = False
    startEnd (Portal "AA") = Start
    startEnd (Portal "ZZ") = End
    startEnd a             = a

findStart = M.foldrWithKey' go (-1, -1)
  where
    go p Start _ = p
    go _ _     p = p

findEnd = M.foldrWithKey' go (-1, -1)
  where
    go p End _ = p
    go _ _   p = p

searcher levelF mx my m (level, (x, y))
    | t == Wall
    = (False, [])
    | t == Open || t == Start
    = (False, map (makeDirs 1 level) (U.dirs (x, y)))
    | isPortal t && nextL >= 0
    = (False, map (makeDirs 0 nextL) (U.dirs connected))
    | t == End && level == 0
    = (True, [])
    | otherwise
    = (False, [])
  where
    makeDirs w l p = (w, (l, p))
    t = fromMaybe Wall $ M.lookup (x, y) m
    isPortal (ConnectedPortal _) = True
    isPortal _                   = False
    ConnectedPortal connected = t
    outer                     = x == 1 || y == 1 || x == mx || y == my
    nextL                     = levelF outer level

solution f xs = show . subtract 2 . fromJust $ M.lookup (0, end) shortest
  where
    m     = buildM xs
    start = findStart m
    end   = findEnd m
    mx    = length . head $ xs
    my    = length xs
    (k, shortest) =
        U.dijkstra (searcher f (mx - 2) (my - 2) m) [(0, (0, start))]

solve1 = solution (flip const)

solve2 = solution levelF
  where
    levelF True  = subtract 1
    levelF False = (+ 1)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
